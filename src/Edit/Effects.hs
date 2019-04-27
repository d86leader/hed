{-# LANGUAGE GADTSyntax #-}
module Edit.Effects
( Buffer(..)  -- |State of editing a text file
, Effects(..) -- |Side effects when editing a file
, EffectAtom  -- |A monad writer that tracks side effects of editing
, EditAtom    -- |Effect atom with buffer embedded. Main return type of edit
              --  functions
, writer, tell, listen, pass -- |Monad writer methods
, runEffects
) where

import Data.Text.Lazy (Text)
import Control.Monad.Writer.Lazy (writer, tell, listen, pass)

-- |A buffer that is modified by commands
data Buffer = Buffer {
     text :: Text
    ,filename :: FilePath
    -- TODO: undo history, redo history
} deriving (Show)

-- |Side effects that can occur when execting commands
data Effects where
    ConsoleLog :: Text -> Effects
    WriteFile :: Buffer -> Effects
    -- something else?
    --
    deriving (Show)

type EffectAtom a = ([Effects], a)
type EditAtom = EffectAtom Buffer

runEffects :: EffectAtom a -> (a, [Effects])
runEffects (x, w) = (w, x)
