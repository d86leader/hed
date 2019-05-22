module Edit.Execute
( runOneCommand
, runCommands
) where


import Control.Monad ((>=>))
import Data.Map.Strict (Map, member)
import Data.Text (Text, pack, append)

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Edit.Command (Command(..), LinewiseMovement(..), CharacterMovement(..)
                    ,VSide(..), HSide(..))
import Edit.Effects (Buffer(..), Effects(..), Cursor (..)
                    ,EditAtom, EffectAtom, Cursor, Cursors
                    ,newCursor, newAllCursors
                    ,editBody, editFileName, editCursors
                    ,writer, tell)
import Util.Text (split2)

-- |Execute editor commands to modify a buffer
runOneCommand :: Command -> Buffer -> EditAtom
runOneCommand (AddLineSelection movement) = addLineSelection movement
runOneCommand (RemoveLineSelection movement) = removeLineSelection movement
runOneCommand (MoveLineSelection movement) = moveLineSelection movement
runOneCommand ResetLineSelection = resetLineSelection

runOneCommand (AddCharacterSelection movement) = addCharacterSelection movement
runOneCommand (ResetCharaterSelection movement) = resetCharacterSelection movement
runOneCommand (MoveCharacterSelection movement) = moveCharacterSelection movement

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
moveLineSelection (AbsoluteNumber number) = -- reset all cursors to one line
    return . safeEditCursors (Map.mapKeys (const number))

resetLineSelection :: Buffer -> EditAtom
resetLineSelection = return . safeEditCursors (const newAllCursors)


--- Character selection commands ---

addCharacterSelection :: CharacterMovement -> Buffer -> EditAtom
addCharacterSelection (Steps offset) =
    return . editCursors (fmap $ addToSide offset)
    where addToSide x (Cursor l r)
            | x == 0  = Cursor l r
            | x < 0   = Cursor (l + x) r
            | x > 0   = Cursor l (r + x)
addCharacterSelection ToBeginning =
    return . editCursors (fmap $ \(Cursor _ r) -> Cursor 0 r)
addCharacterSelection ToEnd =
    return . editCursors (fmap $ \(Cursor l _) -> Cursor l maxBound)
addCharacterSelection _ = badCommand "Movement not supported"


resetCharacterSelection :: CharacterMovement -> Buffer -> EditAtom
resetCharacterSelection ToBeginning =
    return . editCursors (fmap . const $ Cursor 0 0)
resetCharacterSelection _ = badCommand "Movement not supported"

moveCharacterSelection :: CharacterMovement -> Buffer -> EditAtom
moveCharacterSelection (Steps offset) = 
    return . editCursors (fmap $ addToSides offset)
    where addToSides x (Cursor l r) = Cursor (l+x) (r+x)
moveCharacterSelection _ = badCommand "Movement not supported"


--- Line editing commands ---


insertLines :: VSide -> Text -> Buffer -> EditAtom
insertLines side insLine =
    let insert = insertNew side insLine
        shiftCursors = moveCursors side
    in linewiseChange insert shiftCursors
    where
        insertNew :: VSide -> Text -> Cursor -> Text -> [Text]
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


linewiseChange :: (Cursor -> Text -> [Text])
               -> (Cursors -> Cursors)
               -> Buffer -> EditAtom
linewiseChange textEdit cursorEdit buf =
    let cursorLines = Map.toAscList $ bufferCursors buf
        lines = bufferBody buf
        newBody = changeByIndex textEdit cursorLines lines
        size    = length newBody
        newCurs = checkCursors size . cursorEdit $ bufferCursors buf
    in return buf{bufferBody = newBody, bufferCursors = newCurs, bufferSize = size}


changeByIndex :: (Cursor -> Text -> [Text]) -> [(Int, Cursor)] -> [Text] -> [Text]
changeByIndex f ind lines = change 1 ind lines where
    change :: Int -> [(Int, Cursor)] -> [Text] -> [Text]
    change 1 [(1, cur)] []   = f cur (pack "") -- special case for empy buffer
    change _ []  rest = rest
    change curLineNr curs@((curInd, cur):inds) (cline:lines)
        | curLineNr == curInd  =
            (f cur cline) ++ change (curLineNr + 1) inds lines
        | otherwise  = cline : change (curLineNr + 1) curs lines


-- character (inside-line) editing commands


mapWithCursor :: (Text -> Text) -> Cursor -> Text -> Text
mapWithCursor f (Cursor l r) text =
    let (left, mid, right) = split2 (l, r) text
    in left `append` f mid `append` right

characterwiseChange :: (Text -> Text)
                    -> (Cursors -> Cursors)
                    -> Buffer -> EditAtom
characterwiseChange charEdit cursorEdit buf =
    let textEdit c t = [mapWithCursor charEdit c t]
    in linewiseChange textEdit cursorEdit buf





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
