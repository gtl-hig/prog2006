
module Main where

import System.IO
import Math

-- | Main processing function for our input text, into our output text.
-- Input: text from the standard input (finished with CTRL-D).
-- Output: list of ints, without errors (errors handled through Maybe Int)
processAll :: String -> String
processAll text = showStack $ calculate (words text) []

-- | The main entry point.
-- Takes the entire input, passes it to processAll function,
-- prints the outcome, and quits.
main :: IO ()
main = do
    putStr "> "
    hFlush stdout
    line <- getLine
    putStrLn $ processAll line
    if line /= ":q" then main else return () 