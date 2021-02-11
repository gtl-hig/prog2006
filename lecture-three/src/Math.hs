-- This makes the filter nicer, and more compact.
{-# LANGUAGE LambdaCase #-}

module Math(
convString2Maybe,
extractIntOutOfMaybe,
filterInts
) where

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



-- | Given a string of space separated integers (and rubbish)
-- produce a list of integers.
-- 
-- >>> filterInts ""
-- []
--
-- >>> filterInts "1 2 3"
-- [1,2,3]
--
-- >>> filterInts "1 abc 2"
-- [1,2]
--
filterInts :: String -> [Int]
filterInts text = result where
        listOfWords = words text
        listOfInts = map convString2Maybe listOfWords
        result = map extractIntOutOfMaybe $ 
            filter (\case 
                      Nothing -> False
                      Just _ -> True)
                   listOfInts
