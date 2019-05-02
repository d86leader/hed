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
, writer, listen, pass -- |Monad writer methods
, runEffects

, Cursor(..)
, newCursor
) where

import Data.Text (Text)
import Data.Map.Strict (Map)
import Data.Vector (Vector)


-- |Left and right bounds of a cursor on a single line
data Cursor = Cursor Int Int
    deriving (Show)
newCursor = Cursor 0 0


-- |A buffer that is modified by commands
data Buffer = Buffer {
     body :: Vector Text
    ,filename :: FilePath
    ,cursors :: Map Int Cursor
    -- TODO: undo history, redo history
} deriving (Show)
-- Quickly modify buffer content
editBody :: (Vector Text -> Vector Text) -> Buffer -> Buffer
editBody f buf = let text = body buf
                 in buf {body = f text}
editFileName :: (FilePath -> FilePath) -> Buffer -> Buffer
editFileName f buf = let path = filename buf
                 in buf {filename = f path}
editCursors :: (Map Int Cursor -> Map Int Cursor) -> Buffer -> Buffer
editCursors f buf = let cur = cursors buf
                    in buf {cursors = f cur}

-- |Side effects that can occur when execting commands
data Effects where
    ConsoleLog :: Text -> Effects
    WriteFile :: Buffer -> Effects
    -- something else?
    --
    deriving (Show)

data EffectAtom a = EffectAtom [Effects] a
    deriving (Functor)
type EditAtom = EffectAtom Buffer

runEffects :: EffectAtom a -> (a, [Effects])
runEffects (EffectAtom w x) = (x, w)

-- the library implementation of monad writer is not rhs-lazy in the monoid, so
-- we implement all methods ourself
instance Applicative EffectAtom where
    pure x = EffectAtom [] x
    (EffectAtom w1 f) <*> (EffectAtom w2 x) = EffectAtom (w1 ++ w2) (f x)
instance Monad EffectAtom where
    return = pure
    (EffectAtom w1 x) >>= f =
        let EffectAtom w2 y = f x
        in EffectAtom (w1 ++ w2) y
-- and lets declare the writer methods without the class itself
writer :: (a, [Effects]) -> EffectAtom a
writer (x, w) = EffectAtom w x
listen :: EffectAtom a -> EffectAtom (a, [Effects])
listen (EffectAtom w x) = EffectAtom w (x, w)
pass :: EffectAtom (a, [Effects] -> [Effects]) -> EffectAtom a
pass (EffectAtom w (x, f)) = EffectAtom (f w) x
