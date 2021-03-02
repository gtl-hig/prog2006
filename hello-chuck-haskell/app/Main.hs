module Main where

import Lib

main :: IO ()
main = do
  joke <- getNextJoke
  putStrLn joke

