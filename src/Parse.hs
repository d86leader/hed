module Parse
( parseString -- |Lazily produce a series of commands from a string
) where


import Data.Char (isDigit, digitToInt)
import Edit.Command -- import all datatypes


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
parseCommand _ ('g':'g':rest) = ResetLineSelection : parseString rest

-- move line selection
parseCommand 0 ('j':rest)   = (MoveLineSelection (RelativeNumber 1))   : parseString rest
parseCommand rep ('j':rest) = (MoveLineSelection (RelativeNumber rep)) : parseString rest
parseCommand 0 ('k':rest)   = (MoveLineSelection (RelativeNumber $ negate 1))   : parseString rest
parseCommand rep ('k':rest) = (MoveLineSelection (RelativeNumber $ negate rep)) : parseString rest

-- insert new lines
parseCommand 0 ('o':rest)   = parseInsert 1 Bottom rest
parseCommand rep ('o':rest) = parseInsert rep Bottom rest
parseCommand 0 ('O':rest)   = parseInsert 1 Top rest
parseCommand rep ('O':rest) = parseInsert rep Top rest

-- delete lines selected
parseCommand _ ('d':rest) = DeleteLines : parseString rest

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
parseLongCommand _ (_:rest) = BadCommand "Multichar commands are wip, please use them carefully"
                              : parseString rest


---


-- |Some commands require entering a line. This is like vim's insert mode, but the end with <CR>
parseInsert :: Int -> VSide -> String -> [Command]
parseInsert rep side text =
    let (insert', rest') = break (== '\n') text
        insert = replicate rep $ pack insert'
        command = InsertLines side insert
    in case rest' of
        "" -> [command]
        '\n':rest -> [command] : parseString rest
