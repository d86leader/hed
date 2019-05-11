module Edit.Execute
( runOneCommand
, runCommands
) where


import Control.Monad ((>=>))
import Data.Map.Strict (Map, member)
import Data.Text (Text, pack)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Edit.Command (Command(..), LinewiseMovement(..), CharacterMovement(..)
                    ,VSide(..), HSide(..))
import Edit.Effects (Buffer(..), Effects(..)
                    ,EditAtom, EffectAtom, Cursor, Cursors
                    ,newCursor, newAllCursors
                    ,editBody, editFileName, editCursors
                    ,writer, tell)

import Debug.Trace (trace)

-- |Execute editor commands to modify a buffer
runOneCommand :: Command -> Buffer -> EditAtom
runOneCommand (AddLineSelection movement) = addLineSelection movement
runOneCommand (RemoveLineSelection movement) = removeLineSelection movement
runOneCommand (MoveLineSelection movement) = moveLineSelection movement
runOneCommand ResetLineSelection = resetLineSelection

runOneCommand (InsertLines side text) = insertLines side text
runOneCommand DeleteLines = deleteLines
runOneCommand (ChangeLines text) = changeLines text

runOneCommand PrintBufferBody = printBufferBody
runOneCommand WriteBuffer = writeBuffer
runOneCommand (BadCommand errmsg) = badCommand errmsg

-- |Execute a line of commands monadically
runCommands :: [Command] -> Buffer -> EditAtom
runCommands = foldr (>=>) return . map runOneCommand


--- Line selection commands ---

incLines :: (Num a, Eq a) => a -> Map a b -> Map a b
incLines x = Map.fromAscList . map (addToFst x) . Map.toAscList -- mapKeys
    where addToFst x (a, b) = (a + x, b)

addLineSelection :: LinewiseMovement -> Buffer -> EditAtom
addLineSelection (AbsoluteNumber line) =
    return . safeEditCursors (addLine line)
    where addLine line cur = if line `member` cur
                             then cur
                             else Map.insert line newCursor cur
addLineSelection (RelativeNumber offset) =
    return . safeEditCursors (addLines offset)
    where addLines offset cur = let addedCur = incLines offset cur
                                in Map.union cur addedCur

removeLineSelection :: LinewiseMovement -> Buffer -> EditAtom
removeLineSelection (AbsoluteNumber line) =
    return . safeEditCursors (Map.delete line)
removeLineSelection (RelativeNumber offset) =
    return . safeEditCursors (deleteLines offset)
    where deleteLines offset cur =
            let toDel = Set.map (+ offset) $ Map.keysSet cur
            in Map.withoutKeys cur toDel

moveLineSelection :: LinewiseMovement -> Buffer -> EditAtom
moveLineSelection (RelativeNumber offset) =
    return . safeEditCursors (Map.mapKeys (+ offset))
moveLineSelection (AbsoluteNumber _) = \ buf ->
    consoleLog "Can't move selection in absolute nmbers"
    >> return buf

resetLineSelection :: Buffer -> EditAtom
resetLineSelection = return . safeEditCursors (const newAllCursors)


--- Line editing commands ---


insertLines :: VSide -> Text -> Buffer -> EditAtom
insertLines side insLine =
    let insert = insertNew side insLine
        shiftCursors = moveCursors side
    in linewiseChange insert shiftCursors
    where
        insertNew :: VSide -> Text -> Int -> Text -> [Text]
        insertNew Top toIns _ present = [toIns, present]
        insertNew Bottom toIns _ present = [present, toIns]
        --
        moveCursors Bottom = id
        moveCursors Top    = Map.mapKeys (+ 1)


deleteLines :: Buffer -> EditAtom
deleteLines =
    let delete = \_ _ -> []
    in linewiseChange delete id


changeLines :: Text -> Buffer -> EditAtom
changeLines text =
    let change = \_ _ -> [text]
    in linewiseChange change id


linewiseChange :: (Int -> Text -> [Text])
               -> (Cursors -> Cursors)
               -> Buffer -> EditAtom
linewiseChange textEdit cursorEdit buf =
    let cursorLines = map fst . Map.toAscList $ bufferCursors buf
        lines = bufferBody buf
        newBody = changeByIndex textEdit cursorLines lines
        size    = length newBody
        newCurs = checkCursors size . cursorEdit $ bufferCursors buf
    in return buf{bufferBody = newBody, bufferCursors = newCurs, bufferSize = size}


changeByIndex :: (Int -> Text -> [Text]) -> [Int] -> [Text] -> [Text]
changeByIndex f ind lines = change 1 ind lines where
    change :: Int -> [Int] -> [Text] -> [Text]
    change 1 [1] []   = f 1 (pack "") -- special case for empy buffer
    change _ []  rest = rest
    change curLineNr (curInd:inds) (cline:lines)
        | curLineNr == curInd  =
            (f curInd cline) ++ change (curLineNr + 1) inds lines
        | otherwise  = cline : change (curLineNr + 1) (curInd:inds) lines



-- Side-effectful commands

printBufferBody :: Buffer -> EditAtom
printBufferBody buf = writer (buf, [PrintBuffer buf])

writeBuffer :: Buffer -> EditAtom
writeBuffer buf = writer (buf, [WriteFile buf])

badCommand :: String -> Buffer -> EditAtom
badCommand errmsg buf = writer (buf, [ConsoleLog . pack $ errmsg])

consoleLog :: String -> EffectAtom ()
consoleLog x = tell [ConsoleLog $ pack x]


-- Utility stuff

checkCursors :: Int -> Cursors -> Cursors
checkCursors size m =
    let m' = Map.filterWithKey (\k _ -> k > 0 && k <= size) m
    in if Map.size m' == 0
       then newAllCursors
       else m'
-- Edit cursors and check their bounds
safeEditCursors :: (Cursors -> Cursors) -> Buffer -> Buffer
safeEditCursors f buf =
    let size = bufferSize buf
    in editCursors (checkCursors size . f) buf
