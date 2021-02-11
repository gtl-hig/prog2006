
-- This makes the filter nicer, and more compact.
{-# LANGUAGE LambdaCase #-}

module Main where

import Text.Read


-- | Converts an input string into a integer value
-- Handles errors through the use of Maybe Int
--
-- >>> convString2Maybe ""
-- Nothing
--
-- >>> convString2Maybe "dummy string"
-- Nothing
--  
-- >>> convString2Maybe "31"
-- Just 31
--
convString2Maybe :: String -> Maybe Int
convString2Maybe "" = Nothing
convString2Maybe x = readMaybe x :: Maybe Int


-- | Extracts the Int out of Maybe.
-- Note, you should use Maybe Int throughout your processing,
-- as it will simplify composition of your functions. Dealing with
-- Ints through Maybe Ints makes handling error situations super NICE.
-- Extracting "int" value out of Nothing should never happen, 
-- this is why we model it here as Runtime Error.
extractIntOutOfMaybe :: Maybe Int -> Int
extractIntOutOfMaybe Nothing = undefined
extractIntOutOfMaybe (Just num) = num


-- | Main processing function for our input text, into our output text.
-- Input: text from the standard input (finished with CTRL-D).
-- Output: list of ints, without errors (errors handled through Maybe Int)
processAll :: String -> String
processAll text = show filteredListOfInts where
        listOfWords = words text
        listOfInts = map convString2Maybe listOfWords
        filteredListOfInts = map extractIntOutOfMaybe $ 
            filter (\case 
                      Nothing -> False
                      Just _ -> True)
                   listOfInts


-- | The main entry point.
-- Takes the entire input, passes it to processAll function, 
-- prints the outcome, and quits.
main :: IO ()
main = interact processAll