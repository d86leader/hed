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
    let curLinePos = map fst . Map.toAscList $ bufferCursors buf
        lines = zip [1, 2 ..] $ bufferBody buf
        newBody = deepInsert curLinePos lines
        newCurs = moveCursors side $ bufferCursors buf
    in return buf{bufferBody = newBody, bufferCursors = newCurs}
    where
        -- given a list of line numbers, and a list of numbered lines, insert
        -- (already bound) insLine to the correct side (already bound)
        deepInsert :: [Int] -> [(Int, Text)] -> [Text]
        deepInsert [] rest = map snd rest
        -- the variable names are not that good
        -- curN - currently observed cursor line number
        -- curNs - other cursor line numbers
        -- lineN - number of currently observed line
        -- cline - text of currently observed line
        -- lines - list of (number, text) of other lines
        deepInsert (curN:curNs) ((lineN, cline) : lines)
            | curN == lineN  = insertNew side (insLine, cline) $ deepInsert curNs lines
            | otherwise      = cline                           : deepInsert (curN:curNs) lines
        -- a trick for DRY: insertion may top or bottom, and this is how we abstract it
        insertNew :: VSide -> (Text, Text) -> [Text] -> [Text]
        insertNew Top (toIns, textLine) rec = toIns : textLine : rec
        insertNew Bottom (toIns, textLine) rec = textLine : toIns : rec
        --
        moveCursors Bottom = id
        moveCursors Top    = Map.mapKeys (+ 1)


-- Side-effectful commands

printBufferBody :: Buffer -> EditAtom
printBufferBody buf = writer (buf, [PrintBuffer buf])

writeBuffer :: Buffer -> EditAtom
writeBuffer buf = writer (buf, [WriteFile buf])

badCommand :: String -> Buffer -> EditAtom
badCommand errmsg buf = writer (buf, [ConsoleLog . pack $ errmsg])

consoleLog :: String -> EffectAtom ()
consoleLog x = tell [ConsoleLog $ pack x]
