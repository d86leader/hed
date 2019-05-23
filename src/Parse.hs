module Parse
( parseString -- |Lazily produce a series of commands from a string
) where


import Data.Char (isDigit, digitToInt)
import Edit.Command (VSide(..), CharacterMovement(..), LinewiseMovement(..), Command(..))
import Data.Text (Text, pack)
import Data.Char (chr)
import Control.Monad (join)

import qualified Data.Text as Text
import qualified Edit.Command as Command


hleft  = Command.Left
hright = Command.Right

-- my commands are regular, so a simple pattern matching will do for parsing
parseString :: String -> [Command]
parseString str@(char:_)
    | char == '0'   = parseCommand 0 str -- repeats don't start with 0
    | isDigit char  = parseRepeat 0 str
    | otherwise     = parseCommand 0 str
parseString "" = []


---


parseRepeat :: Int -> String -> [Command]
parseRepeat _ "" = [BadCommand "Trying to find repeat on empty input"]
parseRepeat x str@(char:rest)
    | isDigit char  = let digit = digitToInt char
                          x' = x*10 + digit
                      in parseRepeat x' rest
    | otherwise     = parseCommand x str


---


parseCommand :: Int -> String -> [Command]

parseCommand _ "" = [BadCommand "Trying to parse empty input as command"]

-- add line selection
parseCommand 0 ('G':rest)   = (AddLineSelection (AbsoluteNumber 1))   : parseString rest
parseCommand rep ('G':rest) = (AddLineSelection (AbsoluteNumber rep)) : parseString rest
parseCommand 0 ('J':rest)   = (AddLineSelection (RelativeNumber 1))   : parseString rest
parseCommand rep ('J':rest) = (AddLineSelection (RelativeNumber rep)) : parseString rest
parseCommand 0 ('K':rest)   = (AddLineSelection (RelativeNumber $ negate 1))   : parseString rest
parseCommand rep ('K':rest) = (AddLineSelection (RelativeNumber $ negate rep)) : parseString rest
-- reset line selection
parseCommand 0 ('g':'g':rest) = ResetLineSelection : parseString rest
parseCommand rep ('g':'g':rest) = ResetLineSelection : (MoveLineSelection (AbsoluteNumber rep)) : parseString rest

-- add character selection
parseCommand 0   ('H':rest) = (AddCharacterSelection (Steps $ negate 1))   : parseString rest
parseCommand rep ('H':rest) = (AddCharacterSelection (Steps $ negate rep)) : parseString rest
parseCommand 0   ('L':rest) = (AddCharacterSelection (Steps 1))   : parseString rest
parseCommand rep ('L':rest) = (AddCharacterSelection (Steps rep)) : parseString rest
parseCommand _   ('B':rest) = (AddCharacterSelection ToBeginning) : parseString rest
parseCommand _   ('E':rest) = (AddCharacterSelection ToEnd) : parseString rest
-- reset character selection
parseCommand _ ('0':rest) = (ResetCharaterSelection ToBeginning) : parseString rest

-- move line selection
parseCommand 0 ('j':rest)   = (MoveLineSelection (RelativeNumber 1))   : parseString rest
parseCommand rep ('j':rest) = (MoveLineSelection (RelativeNumber rep)) : parseString rest
parseCommand 0 ('k':rest)   = (MoveLineSelection (RelativeNumber $ negate 1))   : parseString rest
parseCommand rep ('k':rest) = (MoveLineSelection (RelativeNumber $ negate rep)) : parseString rest

-- move character selection
parseCommand 0   ('h':rest) = (MoveCharacterSelection (Steps $ negate 1))   : parseString rest
parseCommand rep ('h':rest) = (MoveCharacterSelection (Steps $ negate rep)) : parseString rest
parseCommand 0   ('l':rest) = (MoveCharacterSelection (Steps 1))   : parseString rest
parseCommand rep ('l':rest) = (MoveCharacterSelection (Steps rep)) : parseString rest


-- insert new lines
parseCommand rep ('o':rest) = insertCombinator (InsertLines Bottom) rep rest
parseCommand rep ('O':rest) = insertCombinator (InsertLines Top) rep rest

-- delete lines selected
parseCommand _ ('D':rest) = YankLines : DeleteLines : parseString rest

-- change selected lines
parseCommand rep ('C':rest) = insertCombinator ChangeLines rep rest

-- append text to line
parseCommand rep ('I':rest) = insertCombinator (AppendText hleft) rep rest
parseCommand rep ('A':rest) = insertCombinator (AppendText hright) rep rest

-- delete text selected
parseCommand _ ('d':rest) = YankText : DeleteText : parseString rest

-- insert new text
parseCommand rep ('i':rest) = insertCombinator (InsertText hleft) rep rest
parseCommand rep ('a':rest) = insertCombinator (InsertText hright) rep rest

-- change selected text
parseCommand rep ('c':rest) = insertCombinator ChangeText rep rest


-- yank selected lines
parseCommand rep ('Y':rest) = YankLines : parseString rest
-- yank selected text
parseCommand rep ('y':rest) = YankText : parseString rest
-- put yanked lines
parseCommand rep ('{':rest) = (PutLines Top)    : parseString rest
parseCommand rep ('}':rest) = (PutLines Bottom) : parseString rest
-- put yanked text
parseCommand rep ('[':rest) = (PutText hleft)  : parseString rest
parseCommand rep (']':rest) = (PutText hright) : parseString rest

-- select register
parseCommand _ ('"':name:rest) = (ChangeRegisters name) : parseString rest


-- output
parseCommand _ ('p':rest) = PrintBufferBody : parseString rest
parseCommand rep (':':rest) = parseLongCommand rep rest

-- no command
parseCommand _ ('\n':rest) = parseString rest
parseCommand _ str = [BadCommand $ "Can't parse command in line " ++ str]


---


-- |Some commands can be many symbols. They shall start with colon and end with <CR>
-- This may require a more complex parser in the future
parseLongCommand :: Int -> String -> [Command]

parseLongCommand _ ('p':'r':'i':'n':'t':'\n':rest) = PrintBufferBody : parseString rest
parseLongCommand _ ('w':'\n':rest) = WriteBuffer : parseString rest
parseLongCommand _ ('q':'\n':rest) = [] -- stop parsing
parseLongCommand _ ('r':'e':'g':'s':'\n':rest) = PrintRegisters : parseString rest
parseLongCommand _ (_:rest) = BadCommand "Multichar commands are wip, please use them carefully"
                              : parseString rest


---


-- |Some commands require entering a line. This is like vim's insert mode, but they end with <CR>
-- Returns text entered and registers used
parseInsert :: Int -> String -> ([Either Char Text], String)
parseInsert rep' text =
    let rep = if rep' == 0 then 1 else rep'
        (insert', rest') = break (== '\n') text
        rawText = Text.replicate rep $ pack insert'
        rest = case rest' of
                "" -> ""
                '\n':cs -> cs
        insert:inserts = Text.split (== chr 5) rawText -- split on ^E or C-E
        regNames = map (Left . Text.head) inserts
        texts = map (Right . Text.tail) inserts
        interspersed = join $ zipWith inter regNames texts
    in ((Right insert) : interspersed, rest)
    where inter x y = [x, y]


-- A function to turn the Either above to text commands
commandFromEither :: Either Char Text -> Command
commandFromEither (Left name) = PutTextFrom hleft name
commandFromEither (Right text) = InsertText hleft text


insertCombinator :: (Text -> Command)
                 -> Int -> String -> [Command]
insertCombinator onHead rep str =
    let ((Right first):texts, rest) = parseInsert rep str
        actions = map commandFromEither $ texts
    in (onHead first) : actions ++ parseString rest
