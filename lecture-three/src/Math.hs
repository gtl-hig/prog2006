-- This makes the filter nicer, and more compact.
{-# LANGUAGE LambdaCase #-}

module Math(
convString2Maybe,
extractIntOutOfMaybe,
filterInts,
calculate,
eval,
showStack
) where

import Text.Read
import Data.List (intercalate)



-- | Simple representation of a data stack
type Stack = [Integer]

showStack :: Stack -> String
showStack s = intercalate ", " $ map show s

-- | Simple calculator evaluation function.
--
-- >>> eval "3" []
-- [3]
--
-- >>> eval "*" [2,3]
-- [6]
--
-- >>> eval "+" [2,3]
-- [5]
--
eval :: String -> Stack -> Stack
eval s stack
    | s == "*" = head stack * (stack !! 1) : drop 2 stack
    | s == "+" = head stack + (stack !! 1) : drop 2 stack
    | s == "2^" = 2^ head stack : tail stack
    | otherwise = case convString2Maybe s of
                        Nothing -> stack
                        Just n -> n:stack

-- | Process a list of words (operations or numbers) in the context of a given starting Stack,
-- and return the resulting Stack.
--
-- >>> calculate ["3","2","+"]
-- [5]
--
-- >>> calculate ["10", "3","*"]
-- [30]
--
calculate :: [String] -> Stack
calculate = foldl (flip eval) []

{-- different way of implementing calculate with recursion:

calculate [] stack = stack
calculate (x:xs) stack = calculate xs firstProcessed where
    firstProcessed = eval x stack
 --}

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
convString2Maybe :: String -> Maybe Integer
convString2Maybe "" = Nothing
convString2Maybe x = readMaybe x :: Maybe Integer


-- | Extracts the Int out of Maybe.
-- Note, you should use Maybe Int throughout your processing,
-- as it will simplify composition of your functions. Dealing with
-- Ints through Maybe Ints makes handling error situations super NICE.
-- Extracting "int" value out of Nothing should never happen,
-- this is why we model it here as Runtime Error.
extractIntOutOfMaybe :: Maybe Integer-> Integer
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
filterInts :: String -> [Integer]
filterInts text = result where
        listOfWords = words text
        listOfInts = map convString2Maybe listOfWords
        result = map extractIntOutOfMaybe $
            filter (\case
                      Nothing -> False
                      Just _ -> True)
                   listOfInts
