{-# LANGUAGE GADTSyntax #-}
module Edit.Command where -- export all datatypes

-- |Commands that are parsed from keyboard input or command file

data HSide = Left | Right
data VSide = Top | Bottom

data CharacterMovement where
    OneStep     :: HSide -> CharacterMovement
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
    RemoveLineSelection :: LinewiseMovement -> Command
    ResetLineSelection  :: LinewiseMovement -> Command
    --
    AddCharacterSelection    :: CharacterMovement -> Command
    RemoveCharacterSelection :: CharacterMovement -> Command
    ResetCharaterSelection   :: CharacterMovement -> Command
    --
    DeleteLines :: Command
    ChangeLines :: Command
    YankLines   :: Command
    PutLines    :: VSide -> Command
    InsertLines :: VSide -> Command
    --
    DeleteText :: Command
    ChangeText :: Command
    YankText   :: Command
    PutText    :: HSide -> Command
    InsertText :: HSide -> Command
    --
    Undo :: Command
    Redo :: Command
