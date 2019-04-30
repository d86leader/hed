module Edit.Execute
(
) where


import Data.Map.Strict (Map, member, insert, union, delete, keysSet, keys)
import Data.Text (append, snoc)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Edit.Command as Command
import Edit.Command (Command(..), LinewiseMovement(..), CharacterMovement(..)
                    ,VSide(..), HSide(..))
import Edit.Effects (Buffer(..), Effects(..), EditAtom, newCursor
                    ,editBody, editFileName, editCursors
                    ,writer)


-- |Execute editor commands to modify a buffer

runCommand :: Command -> Buffer -> EditAtom
runCommand (AddLineSelection movement) = addLineSelection movement
runCommand (RemoveLineSelection movement) = removeLineSelection movement

runCommand DeleteLines = deleteLines

runCommand PrintBufferBody = printBufferBody
runCommand WriteBuffer = writeBuffer


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


--- Line editing commands ---

deleteLines :: Buffer -> EditAtom
deleteLines Buffer{body = lines , cursors = cur, filename = fname} =
    let numbers = keys cur
        dropAll = foldr (.) id (map Vector.drop numbers)
        dropped = dropAll lines
        resetCursor = Map.fromAscList [(1, newCursor)]
    in return Buffer{body = dropped, cursors = resetCursor, filename = fname}


-- Side-effectful commands

printBufferBody :: Buffer -> EditAtom
printBufferBody buf =
    let text = foldr joinLines Text.empty $ body buf
    in writer (buf, [ConsoleLog text])
    where left `joinLines` right = left `snoc` '\n' `append` right

writeBuffer :: Buffer -> EditAtom
writeBuffer buf = writer (buf, [WriteFile buf])
