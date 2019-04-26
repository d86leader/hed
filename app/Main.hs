module Main where

import Data.Text.Lazy as T
import Data.Text.Lazy.IO as TIO

main :: IO ()
main = TIO.putStrLn (T.pack "Hello stack")
