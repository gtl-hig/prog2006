
-- This makes the filter nicer, and more compact.
{-# LANGUAGE LambdaCase #-}

module Main where

import Text.Read


-- | Converts an input string into a integer value
-- Handles errors through the use of Maybe Int
--
-- >>> conv ""
-- Nothing
--
-- >>> conv "dummy string"
-- Nothing
--  
-- >>> conv "31"
-- Just 31
--
conv :: String -> Maybe Int
conv "" = Nothing
conv x = readMaybe x :: Maybe Int

-- | Extracts the Int out of Maybe.
-- Note, you should use Maybe Int throughout your processing,
-- as it will simplify composition of your functions. Dealing with
-- Ints is "risky" as there might be error situations, so dealing with 
-- Maybe Ints is usually preferred. Extracting "int" value out of Nothing
-- should never happen, this is why we model it here as Runtime Error.
ex :: Maybe Int -> Int
ex Nothing = undefined
ex (Just num) = num


-- | Main processing function for our input text, into our output text.
-- Input: text from the standard input (finished with CTRL-D).
-- Output: list of ints, without errors (errors handled through Maybe Int)
processAll :: String -> String
processAll text = show filteredListOfInts where
        listOfWords = words text
        listOfInts = map conv listOfWords
        filteredListOfInts = map ex $ 
            filter (\case 
                      Nothing -> False
                      Just _ -> True)
                   listOfInts


-- | The main entry point.
-- Takes the entire input, passes it to processAll function, 
-- prints the outcome, and quits.
main :: IO ()
main = interact processAll