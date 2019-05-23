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
                    ,editFileName, editCursors
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
        shift = moveCursors side
    in changeKeepPositions' insert shift 1
    where
        insertNew :: VSide -> Text -> (a -> Text -> [Text])
        insertNew Top toIns _ present = [toIns, present]
        insertNew Bottom toIns _ present = [present, toIns]
        --
        moveCursors :: VSide -> ((Int, Cursor) -> Text -> [(Int, Cursor)])
        moveCursors Bottom (line, cur) _ = [(line + 1, cur)]
        moveCursors Top    (line, cur) _ = [(line, cur)]


deleteLines :: Buffer -> EditAtom
deleteLines =
    let delete _ _ = []
        shift (line, cur) _ = [(line - 1, cur)]
    in changeKeepPositions' delete shift (negate 1)


changeLines :: Text -> Buffer -> EditAtom
changeLines text =
    let change _ _ = text
    in wholeLineChange change id


appendText :: HSide -> Text -> Buffer -> EditAtom
appendText side text = wholeLineChange (change side text) (move side)
    where
    change :: HSide -> Text -> a -> Text -> Text
    change Left  new _ old = new `append` old
    change Right new _ old = old `append` new
    --
    move Right = id
    move Left = \(Cursor l r) -> Cursor (l + Text.length text) (r + Text.length text)

putLines :: VSide -> Buffer -> EditAtom
putLines side buf =
    changeKeepPositions (insert side) (shift side) context 1 buf
    where
    insert :: VSide -> (Text, a) -> Text -> [Text]
    insert Top (new, _) old = [new, old]
    insert Bottom (new, _) old = [old, new]
    --
    shift :: VSide -> (Text, Int) -> Text -> [(Int, Cursor)]
    shift Top    (_, line) _ = [(line, Cursor 0 maxBound)]
    shift Bottom (_, line) _ = [(line + 1, Cursor 0 maxBound)]
    --
    context = zip (getUnnamedInf buf) (map fst . getCursors $ buf)


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
    let (lines, cursors) = unzip . getCursors $ buf
        context = zip lines (zip cursors (getRegisterInf name buf))
        body = bufferBody buf
        --
        (body', cursors') = inline changeByIndex (insertReg side) (contextEdit side) context body
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
    context = zip (map snd . getCursors $ buf) (getRegisterInf name buf)
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
    let inds = map fst . getCursors $ buf
        lines = getByIndex inds . bufferBody $ buf
    in return . setUnnamed lines $ buf

yankText :: Buffer -> EditAtom
yankText buf =
    let curs = getCursors buf
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


-- Line editing combinator. It maps each line with some context, and only maps
-- those lines for which indexed contexts exist.
changeByIndex :: (a -> Text -> [Text]) -- text editor
              -> (a -> Text -> [b]) -- context editor
              -> [(Int, a)] -- line numbers to edit and context for each line
              -> [Text] -> ([Text], [b])
changeByIndex textEdit contEdit context lines =
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

-- combinator above with trivial context editor
changeByIndex' :: (a -> Text -> [Text]) -- text editor
              -> [(Int, a)] -- line numbers to edit and context for each line
              -> [Text] -> [Text]
changeByIndex' f ind = fst . inline changeByIndex f (\x _ -> [x]) ind


-- Main line editing combinator; Wraps changeByIndex nicely into editAtom
linewiseChangeCombinator :: (a -> Text -> [Text]) -- text editor
                         -> (a -> Text -> [(Int, Cursor)]) -- cursor editor
                         -> [a] -- context
                         -> Buffer -> EditAtom
linewiseChangeCombinator textEdit contextEdit context buf =
    let inds = map fst . getCursors $ buf
        fullContext = zip inds context
        (newBody, newCurs) = changeByIndex textEdit contextEdit fullContext $ bufferBody buf
        size    = length newBody
        newCurs' = prepareCursors size newCurs
    in return buf{bufferBody = newBody, bufferCursors = newCurs', bufferSize = size}


-- Change only one line
wholeLineChange :: (Cursor -> Text -> Text)
               -> (Cursor -> Cursor)
               -> Buffer -> EditAtom
wholeLineChange textEdit cursorEdit buf =
    let textEdit' (_, c) t = [textEdit c t]
        cursorEdit' (line, cur) _ = [(line, cursorEdit cur)]
        context = getCursors buf
    in linewiseChangeCombinator textEdit' cursorEdit' context buf


-- Line editing combinator for actions that can add or remove lines
changeKeepPositions :: (a -> Text -> [Text]) -- text editor
                    -> (a -> Text -> [(Int, Cursor)]) -- cursor editor
                    -> [a] -> Int -- context and amount of lines inserted for each
                    -> Buffer -> EditAtom
changeKeepPositions textEdit cursorEdit context insam buf =
    inline linewiseChangeCombinator textEdit' cursorEdit' context' buf
    where
    shifts = map (* insam) [0, 1 ..]
    context' = zip shifts context
    --
    onFst f (x, y) = (f x, y)
    cursorEdit' (shift, a) text =
        let curs = cursorEdit a text
        in map (onFst (+ shift)) curs
    --
    textEdit' (_, a) = textEdit a

-- combinator above specified for cursor context
changeKeepPositions' :: ((Int, Cursor) -> Text -> [Text])
                     -> ((Int, Cursor) -> Text -> [(Int, Cursor)])
                     -> Int
                     -> Buffer -> EditAtom
changeKeepPositions' textEdit cursorEdit insam buf =
    let context = getCursors buf
    in inline changeKeepPositions textEdit cursorEdit context insam buf


-- Get lines; indexed from 1
getByIndex :: [Int] -> [Text] -> [Text]
getByIndex = getByIndex' 1 where
    getByIndex' :: Int -> [Int] -> [Text] -> [Text]
    getByIndex' 1 [1] [] = [pack ""] -- special case for empty buffer
    getByIndex' _ []  _  = []
    getByIndex' current (ind:inds) (line:lines)
        | current == ind  = line : getByIndex' (current + 1) inds lines
        | otherwise  = getByIndex' (current + 1) (ind:inds) lines


-- Main character editing combinator. Change only selected text in each line
characterwiseChange :: (Text -> Text)
                    -> (Cursor -> Cursor)
                    -> Buffer -> EditAtom
characterwiseChange charEdit oneCursorEdit buf =
    let textEdit = mapSelected charEdit
        cursorEdit = oneCursorEdit
    in wholeLineChange textEdit cursorEdit buf
    where
    -- Character editing combinator that changes one line with cursor
    mapSelected :: (Text -> Text) -> Cursor -> Text -> Text
    mapSelected f (Cursor l r) text =
        let (left, mid, right) = split2 (l, r) text
        in left `append` f mid `append` right


-- get infinite register suitable for any put
getRegisterInf :: Char -> Buffer -> [Text]
getRegisterInf name buf = let cont = getRegister name buf
                          in cont ++ repeat (last cont)

-- oprations on unnamed register
getUnnamed = getRegister '"'
setUnnamed = setRegister '"'
editUnnamed = editRegister '"'
getUnnamedInf = getRegisterInf '"'

-- operations on buffer cursors
getCursors = Map.toAscList . bufferCursors
prepareCursors size = checkCursors size . Map.fromAscList
