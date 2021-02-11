
module Main where

import Math

-- | Main processing function for our input text, into our output text.
-- Input: text from the standard input (finished with CTRL-D).
-- Output: list of ints, without errors (errors handled through Maybe Int)
processAll :: String -> String
processAll text = show (filterInts text)

-- | The main entry point.
-- Takes the entire input, passes it to processAll function,
-- prints the outcome, and quits.
main :: IO ()
main = interact processAll