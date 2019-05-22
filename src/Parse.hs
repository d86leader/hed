module Parse
( parseString -- |Lazily produce a series of commands from a string
) where


import Data.Char (isDigit, digitToInt)
import Edit.Command -- import all datatypes
import Data.Text (Text, pack)

import qualified Data.Text as Text

import Prelude hiding (Left, Right)


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
parseCommand rep ('o':rest) =
    let (text, rest') = parseInsert rep rest
    in (InsertLines Bottom text) : parseString rest'
parseCommand rep ('O':rest) =
    let (text, rest') = parseInsert rep rest
    in (InsertLines Top text) : parseString rest'

-- delete lines selected
parseCommand _ ('D':rest) = YankLines : DeleteLines : parseString rest

-- change selected lines
parseCommand rep ('C':rest) =
    let (text, rest') = parseInsert rep rest
    in (ChangeLines text) : parseString rest'

-- append text to line
parseCommand rep ('I':rest) = 
    let (text, rest') = parseInsert rep rest
    in (AppendText Left text) : parseString rest'
parseCommand rep ('A':rest) = 
    let (text, rest') = parseInsert rep rest
    in (AppendText Right text) : parseString rest'

-- delete text selected
parseCommand _ ('d':rest) = YankText : DeleteText : parseString rest

-- insert new text
parseCommand rep ('i':rest) = 
    let (text, rest') = parseInsert rep rest
    in (InsertText Left text) : parseString rest'
parseCommand rep ('a':rest) = 
    let (text, rest') = parseInsert rep rest
    in (InsertText Right text) : parseString rest'

-- change selected text
parseCommand rep ('c':rest) = 
    let (text, rest') = parseInsert rep rest
    in (ChangeText text) : parseString rest'


-- yank selected lines
parseCommand rep ('Y':rest) = YankLines : parseString rest
-- yank selected text
parseCommand rep ('y':rest) = YankText : parseString rest
-- put yanked lines
parseCommand rep ('{':rest) = (PutLines Top)    : parseString rest
parseCommand rep ('}':rest) = (PutLines Bottom) : parseString rest
-- put yanked text
parseCommand rep ('[':rest) = (PutText Left)  : parseString rest
parseCommand rep (']':rest) = (PutText Right) : parseString rest

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
parseInsert :: Int -> String -> (Text, String)
parseInsert rep' text =
    let rep = if rep' == 0 then 1 else rep'
        (insert', rest') = break (== '\n') text
        insert = Text.replicate rep $ pack insert'
        rest = case rest' of
                "" -> ""
                '\n':cs -> cs
    in (insert, rest)
