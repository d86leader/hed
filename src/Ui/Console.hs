{-# LANGUAGE OverloadedStrings #-}
module Ui.Console
( runUi
) where


import Data.Vector (fromList)
import Data.Text (unpack)

import qualified Data.Map as Map

import Parse (parseString)
import Edit.Execute (runCommands)
import Edit.Effects (Buffer(..), Effects(..), newCursor, runEffects)


-- |Get user input from command line, parse it, execute it in free context, and
-- run this context to IO
runUi :: IO ()
runUi = do
    input <- getContents
    let buffer = Buffer{body = fromList ["foobar", "barbaz", "keklol", "gotcha"]
                       ,filename = "None"
                       ,cursors = Map.fromAscList [(1, newCursor)]
                       }
    let commands = parseString input
    let executed = runCommands commands buffer
    let effects = snd . runEffects $ executed
    evalEffects effects


runOneEffect :: Effects -> IO ()
runOneEffect (ConsoleLog text) = putStrLn . unpack $ text
runOneEffect (WriteFile buf) = putStrLn $ "(Pretend) file " ++ filename buf ++ " written"

evalEffects :: [Effects] -> IO ()
evalEffects = mapM_ runOneEffect
