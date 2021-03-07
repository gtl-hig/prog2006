module Main where

import Lib

main :: IO ()
main = execCmd [] >> return ()
    