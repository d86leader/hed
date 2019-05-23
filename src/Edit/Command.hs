{-# LANGUAGE GADTSyntax #-}
module Edit.Command
( HSide(..), VSide(..)
, CharacterMovement(..), LinewiseMovement(..)
, Command(..)
) where


import Data.Text (Text)


-- |Commands that are parsed from keyboard input or command file

data HSide = Left | Right
data VSide = Top | Bottom

data CharacterMovement where
    Steps       :: Int -> CharacterMovement
    FindSymbol  :: Char -> HSide -> CharacterMovement
    ToSymbol    :: Char -> HSide -> CharacterMovement
    ToBeginning :: CharacterMovement
    ToEnd       :: CharacterMovement

data LinewiseMovement where
    AbsoluteNumber :: Int -> LinewiseMovement
    RelativeNumber :: Int -> LinewiseMovement
    -- TODO: all matching regex


data Command where
    AddLineSelection    :: LinewiseMovement -> Command
    RemoveLineSelection :: LinewiseMovement -> Command -- what's the syntax for this?
    ResetLineSelection  :: Command
    MoveLineSelection :: LinewiseMovement -> Command
    --
    AddCharacterSelection    :: CharacterMovement -> Command
    RemoveCharacterSelection :: CharacterMovement -> Command
    ResetCharaterSelection   :: CharacterMovement -> Command -- what does this do?
    MoveCharacterSelection   :: CharacterMovement -> Command
    --
    DeleteLines :: Command
    ChangeLines :: Text -> Command
    YankLines   :: Command
    PutLines    :: VSide -> Command
    InsertLines :: VSide -> Text -> Command
    --
    AppendText :: HSide -> Text -> Command
    --
    DeleteText :: Command
    ChangeText :: Text -> Command
    YankText   :: Command
    PutText    :: HSide -> Command
    PutTextFrom :: HSide -> Char -> Command
    InsertText :: HSide -> Text -> Command
    AppendPutText :: HSide -> Command
    --
    Undo :: Command
    Redo :: Command
    --
    ChangeRegisters :: Char -> Command -- swap unnamed and named registers
    --
    PrintBufferBody :: Command
    PrintRegisters  :: Command
    WriteBuffer     :: Command
    --
    BadCommand :: String -> Command -- contains error message
