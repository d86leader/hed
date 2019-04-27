{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text.Lazy as T
import Data.Text.Lazy.IO as TIO
import Edit.Effects

main :: IO ()
main =
    let (r, m) = runEffects writerStuff
    in print r
    >> print m

writerStuff :: EffectAtom Integer
writerStuff = do
    tell [ConsoleLog "Hello"]
    first <- writer (2, [ConsoleLog "Putting 0"])
    return $ first + 2
