{-# LANGUAGE GADTSyntax, DeriveFunctor #-}
module Edit.Effects
( Buffer(..)  -- |State of editing a text file
, editBody
, editFileName
, editCursors

, Effects(..) -- |Side effects when editing a file
, EffectAtom  -- |A monad writer that tracks side effects of editing
, EditAtom    -- |Effect atom with buffer embedded. Main return type of edit
              --  functions
, writer, tell, listen, pass -- |Monad writer methods
, runEffects

, Cursor(..), Cursors
, newCursor, newAllCursors
) where

import Data.Text (Text)
import Data.Map.Strict (Map, fromAscList)
import Control.Monad.Writer.Lazy (Writer, writer, tell, listen, pass
                                 ,runWriter)


-- |Left and right bounds of a cursor on a single line.
-- Inclusive, which means that if you delete selected (Cursor l r) text it will
-- delete character at l and character at r.
data Cursor = Cursor Int Int
    deriving (Show)
newCursor :: Cursor
newCursor = Cursor 0 0
newAllCursors :: Map Int Cursor
newAllCursors = fromAscList [(1, newCursor)]
-- For ease of function types
type Cursors = Map Int Cursor

-- Named registers containing a line for each cursor (or less)
type Registers = Map Char [Text]


-- |A buffer that is modified by commands
data Buffer = Buffer {
     bufferBody      :: [Text]
    ,bufferFilename  :: FilePath
    ,bufferCursors   :: Cursors
    ,bufferSize      :: Int -- amount of lines
    ,bufferRegisters :: Registers
    -- TODO: undo history, redo history
} deriving (Show)
-- Quickly modify buffer content
editBody :: ([Text] -> [Text]) -> Buffer -> Buffer
editBody f buf = let text = bufferBody buf
                 in buf {bufferBody = f text}
editFileName :: (FilePath -> FilePath) -> Buffer -> Buffer
editFileName f buf = let path = bufferFilename buf
                 in buf {bufferFilename = f path}
editCursors :: (Cursors -> Cursors) -> Buffer -> Buffer
editCursors f buf = let cur = bufferCursors buf
                    in buf {bufferCursors = f cur}

-- |Side effects that can occur when execting commands
data Effects where
    ConsoleLog :: Text -> Effects
    WriteFile :: Buffer -> Effects
    PrintBuffer :: Buffer -> Effects
    -- something else?
    --
    deriving (Show)

type EffectAtom a = Writer [Effects] a
type EditAtom = EffectAtom Buffer

runEffects :: EffectAtom a -> (a, [Effects])
runEffects = runWriter
