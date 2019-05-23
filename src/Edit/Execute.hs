module Edit.Execute
( runOneCommand
, runCommands
) where


import Control.Monad ((>=>))
import Data.Map.Strict (Map, member)
import Data.Text (Text, pack, append, empty)
import GHC.Exts (inline)

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import Edit.Command (Command(..), LinewiseMovement(..), CharacterMovement(..)
                    ,VSide(..), HSide(..))
import Edit.Effects (Buffer(..), Effects(..), Cursor (..)
                    ,EditAtom, EffectAtom, Cursor, Cursors
                    ,newCursor, newAllCursors
                    ,editBody, editFileName, editCursors, editSize
                    ,setRegister ,getRegister, editRegister
                    ,writer, tell)
import Util.Text (split2)

import Prelude hiding (Left, Right)


-- |Execute editor commands to modify a buffer
runOneCommand :: Command -> Buffer -> EditAtom

runOneCommand (AddLineSelection movement)    = addLineSelection movement
runOneCommand (RemoveLineSelection movement) = removeLineSelection movement
runOneCommand (MoveLineSelection movement)   = moveLineSelection movement
runOneCommand ResetLineSelection             = resetLineSelection

runOneCommand (AddCharacterSelection movement)  = addCharacterSelection movement
runOneCommand (ResetCharaterSelection movement) = resetCharacterSelection movement
runOneCommand (MoveCharacterSelection movement) = moveCharacterSelection movement

runOneCommand (InsertLines side text) = insertLines side text
runOneCommand DeleteLines             = deleteLines
runOneCommand (ChangeLines text)      = changeLines text

runOneCommand (AppendText side text) = appendText side text

runOneCommand DeleteText             = deleteText
runOneCommand (ChangeText text)      = changeText text
runOneCommand (InsertText side text) = insertText side text

runOneCommand YankLines = yankLines
runOneCommand YankText  = yankText
runOneCommand (PutLines side) = putLines side
runOneCommand (PutText side)  = putText side
runOneCommand (PutTextFrom side name) = putTextFrom side name

runOneCommand (ChangeRegisters name) = changeRegisters name

runOneCommand PrintBufferBody     = printBufferBody
runOneCommand PrintRegisters      = printRegisters
runOneCommand WriteBuffer         = writeBuffer
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
        insertNew :: VSide -> Text -> a -> Text -> [Text]
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


appendText :: HSide -> Text -> Buffer -> EditAtom
appendText side text = linewiseChange (change side text) (fmap (move side))
    where
    change :: HSide -> Text -> a -> Text -> [Text]
    change Left  new _ old = [new `append` old]
    change Right new _ old = [old `append` new]
    --
    move Right = id
    move Left = \(Cursor l r) -> Cursor (l + Text.length text) (r + Text.length text)

putLines :: VSide -> Buffer -> EditAtom
putLines side buf =
    linewiseChangeCombinator context (insert side) id buf where
    insert :: VSide -> Text -> Text -> [Text]
    insert Top new old = [new, old]
    insert Bottom new old = [old, new]
    --
    context = getUnnamedInf buf


-- character (inside-line) editing commands


deleteText :: Buffer -> EditAtom
deleteText = characterwiseChange (const empty) toLeft
    where toLeft (Cursor l _) = Cursor l l

changeText :: Text -> Buffer -> EditAtom
changeText text = characterwiseChange (const text) adjust
    where adjust (Cursor l _) = Cursor l (l + Text.length text - 1)

insertText :: HSide -> Text -> Buffer -> EditAtom
insertText side text =
    characterwiseChange (insert side text) (adjust side)
    where
    insert :: HSide -> Text -> Text -> Text
    insert Left  new old = new `append` old
    insert Right new old = old `append` new
    --
    adjust Left  (Cursor l r) = Cursor l (l + Text.length text - 1)
    adjust Right (Cursor l r) = Cursor (r + 1) (r + Text.length text)

putText :: HSide -> Buffer -> EditAtom
putText side = putTextFrom side '"'

putTextFrom :: HSide -> Char -> Buffer -> EditAtom
putTextFrom side name buf =
    let (lines, cursors) = unzip . Map.toAscList $ bufferCursors buf
        context = zip lines (zip cursors (getRegisterInf name buf))
        body = bufferBody buf
        --
        (body', cursors') = inline changeByIndex' (insertReg side) (contextEdit side) context body
        cursorMap = Map.fromAscList $ zip lines cursors'
    in return buf{bufferBody = body', bufferCursors = cursorMap}
    where
    insertReg :: HSide -> (Cursor, Text) -> Text -> [Text]
    insertReg side (Cursor l r, new) old =
        let (left, mid, right) = split2 (l, r) old
        in case side of
            Left  -> [left `append` new `append` mid `append` right]
            Right -> [left `append` mid `append` new `append` right]
    --
    context = zip (map snd . Map.toAscList . bufferCursors $ buf) (getRegisterInf name buf)
    --
    contextEdit :: HSide -> (Cursor, Text) -> Text -> [Cursor]
    contextEdit side (Cursor l r, new) old =
        let size = Text.length new
        in case side of
            Left  -> [Cursor l (l + size - 1)]
            Right -> [Cursor (r + 1) (r + size)]


-- yank commands

yankLines :: Buffer -> EditAtom
yankLines buf =
    let inds = map fst . Map.toAscList . bufferCursors $ buf
        lines = getByIndex inds . bufferBody $ buf
    in return . setUnnamed lines $ buf

yankText :: Buffer -> EditAtom
yankText buf =
    let curs = Map.toAscList . bufferCursors $ buf
        inds = map fst curs
        poss = map (\(Cursor l r) -> (l, r)) . map snd $ curs
        lines = getByIndex inds . bufferBody $ buf
        parts = zipWith split2 poss lines
        texts = map (\(_, y, _) -> y) parts
    in return . setUnnamed texts $ buf


changeRegisters :: Char -> Buffer -> EditAtom
changeRegisters name buf =
    let unnamed = getUnnamed buf
        named   = getRegister name buf
        buf' = setRegister name unnamed $ setUnnamed named buf
    in return buf'


-- Side-effectful commands

printBufferBody :: Buffer -> EditAtom
printBufferBody buf = writer (buf, [PrintBuffer buf])

printRegisters :: Buffer -> EditAtom
printRegisters buf =
    let regs = Map.toAscList . bufferRegisters $ buf
        pretty = map printReg regs
        printEff = mapM_ tell pretty
    in printEff >> return buf
    where printReg :: (Char, [Text]) -> [Effects]
          printReg (name, cont) =
            let header = pack $ '"':name:' ':[]
                first = header `append` head cont
                rest  = map (append $ pack "   ") $ tail cont
                bodies = first:rest
            in map ConsoleLog bodies

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


-- Main line editing combinator; Wraps changeByIndex nicely into editAtom
linewiseChangeCombinator :: [a]
                         -> (a -> Text -> [Text])
                         -> (Cursors -> Cursors)
                         -> Buffer -> EditAtom
linewiseChangeCombinator context textEdit cursorEdit buf =
    let inds = map fst . Map.toAscList . bufferCursors $ buf
        fullContext = zip inds context
        newBody = changeByIndex textEdit fullContext $ bufferBody buf
        size    = length newBody
        newCurs = checkCursors size . cursorEdit $ bufferCursors buf
    in return buf{bufferBody = newBody, bufferCursors = newCurs, bufferSize = size}


-- Line editing combinator for many functions above. Uses cursors for context
linewiseChange :: (Cursor -> Text -> [Text])
               -> (Cursors -> Cursors)
               -> Buffer -> EditAtom
linewiseChange textEdit cursorEdit buf =
    let cursorLines = map snd . Map.toAscList $ bufferCursors buf
    in inline linewiseChangeCombinator cursorLines textEdit cursorEdit buf

-- linewiseChange' :: (Cursor -> Text -> [Text])
--                -> Buffer -> EditAtom
-- linewiseChange' textEdit cursorEdit buf =
--     let cursorLines = map snd . Map.toAscList $ bufferCursors buf
--     in inline linewiseChangeCombinator cursorLines textEdit cursorEdit buf
--     where
--     contextEdit :: Cursor -> Text -> [Cursor]


-- Line editing combinator. It maps each line with some context, and only maps
-- those lines for which indexed contexts exist.
changeByIndex' :: (a -> Text -> [Text]) -- text editor
              -> (a -> Text -> [b]) -- context editor
              -> [(Int, a)] -- line numbers to edit and context for each line
              -> [Text] -> ([Text], [b])
changeByIndex' textEdit contEdit context lines =
    let (inds, conts) = unzip context
    in  change 1 inds conts lines
    where
    -- special case which only happens in empty buffer
    change 1 [1] [x] [] = (textEdit x empty, contEdit x empty)
    -- base: no more lines selected
    change _ [] _ restLines = (restLines, [])
    -- main recursive case
    change curLineNr (ind:inds) (cont:conts) (line:lines)
        | curLineNr == ind  =
            let (recLines, recConts) = change (curLineNr + 1) inds conts lines
                curLines = textEdit cont line
                curConts = contEdit cont line
            in (curLines ++ recLines, curConts ++ recConts)
        | otherwise  =
            let (recLines, recConts) =
                    change (curLineNr + 1) (ind:inds) (cont:conts) lines
            in (line:recLines, recConts)

changeByIndex :: (a -> Text -> [Text]) -- text editor
              -> [(Int, a)] -- line numbers to edit and context for each line
              -> [Text] -> [Text]
changeByIndex f ind = fst . changeByIndex' f (\x _ -> [x]) ind


-- Get lines; indexed from 1
getByIndex :: [Int] -> [Text] -> [Text]
getByIndex = getByIndex' 1 where
    getByIndex' :: Int -> [Int] -> [Text] -> [Text]
    getByIndex' 1 [1] [] = [pack ""] -- special case for empty buffer
    getByIndex' _ []  _  = []
    getByIndex' current (ind:inds) (line:lines)
        | current == ind  = line : getByIndex' (current + 1) inds lines
        | otherwise  = getByIndex' (current + 1) (ind:inds) lines


-- Character editing combinator that changes one line with cursor
mapWithCursor :: (Text -> Text) -> Cursor -> Text -> Text
mapWithCursor f (Cursor l r) text =
    let (left, mid, right) = split2 (l, r) text
    in left `append` f mid `append` right


-- Main character editing combinator
characterwiseChange :: (Text -> Text)
                    -> (Cursor -> Cursor)
                    -> Buffer -> EditAtom
characterwiseChange charEdit oneCursorEdit buf =
    let textEdit c t = [mapWithCursor charEdit c t]
        cursorEdit = fmap oneCursorEdit
    in linewiseChange textEdit cursorEdit buf


-- get infinite register suitable for any put
getRegisterInf :: Char -> Buffer -> [Text]
getRegisterInf name buf = let cont = getRegister name buf
                          in cont ++ repeat (last cont)

-- oprations on unnamed register
getUnnamed = getRegister '"'
setUnnamed = setRegister '"'
editUnnamed = editRegister '"'
getUnnamedInf = getRegisterInf '"'
