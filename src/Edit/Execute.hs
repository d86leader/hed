module Edit.Execute
( runOneCommand
, runCommands
) where


import Control.Monad ((>=>))
import Data.Map.Strict (Map, member, insert, union, delete, keysSet, keys)
import Data.Text (Text, pack)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Edit.Command (Command(..), LinewiseMovement(..), CharacterMovement(..)
                    ,VSide(..), HSide(..))
import Edit.Effects (Buffer(..), Effects(..), EditAtom, EffectAtom
                    ,newCursor, newAllCursors
                    ,editBody, editFileName, editCursors
                    ,writer, tell)


-- |Execute editor commands to modify a buffer
runOneCommand :: Command -> Buffer -> EditAtom
runOneCommand (AddLineSelection movement) = addLineSelection movement
runOneCommand (RemoveLineSelection movement) = removeLineSelection movement
runOneCommand (MoveLineSelection movement) = moveLineSelection movement
runOneCommand ResetLineSelection = resetLineSelection

runOneCommand (InsertLines side text) = insertLines side text

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
    return . editCursors (addLine line)
    where addLine line cur = if line `member` cur
                             then cur
                             else insert line newCursor cur
addLineSelection (RelativeNumber offset) =
    return . editCursors (addLines offset)
    where addLines offset cur = let addedCur = incLines offset cur
                                in union cur addedCur

removeLineSelection :: LinewiseMovement -> Buffer -> EditAtom
removeLineSelection (AbsoluteNumber line) =
    return . editCursors (delete line)
removeLineSelection (RelativeNumber offset) =
    return . editCursors (deleteLines offset)
    where deleteLines offset cur =
            let toDel = Set.map (+ offset) $ keysSet cur
            in Map.withoutKeys cur toDel

moveLineSelection :: LinewiseMovement -> Buffer -> EditAtom
moveLineSelection (RelativeNumber offset) =
    return . editCursors (Map.mapKeys (+ offset))
moveLineSelection (AbsoluteNumber _) = \ buf ->
    consoleLog "Can't move selection in absolute nmbers"
    >> return buf

resetLineSelection :: Buffer -> EditAtom
resetLineSelection = return . editCursors (const newAllCursors)


--- Line editing commands ---


insertLines :: VSide -> Text -> Buffer -> EditAtom
insertLines side insLine buf =
    let cursorLines = map fst . Map.toAscList $ bufferCursors buf
        lines = bufferBody buf
        insert = insertNew side insLine
        newBody = changeByIndex insert cursorLines lines
        newCurs = moveCursors side $ bufferCursors buf
    in return buf{bufferBody = newBody, bufferCursors = newCurs}
    where
        insertNew :: VSide -> Text -> Int -> Text -> [Text]
        insertNew Top toIns _ present = [toIns, present]
        insertNew Bottom toIns _ present = [present, toIns]
        --
        moveCursors Bottom = id
        moveCursors Top    = Map.mapKeys (+ 1)


changeByIndex :: (Int -> Text -> [Text]) -> [Int] -> [Text] -> [Text]
changeByIndex f ind lines = change 1 ind lines where
    change :: Int -> [Int] -> [Text] -> [Text]
    change _ [] rest = rest
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
