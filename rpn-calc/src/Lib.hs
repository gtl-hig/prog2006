module Lib
    ( processLine
    , processToken
    ) where

import Text.Read (readMaybe)
import Control.Monad.State.Lazy (State, get, put, evalState, runState, execState, replicateM, when, unless)




-- | Process a line of input, and produce a string output
--
processLine :: String -> String
processLine line = unwords $ map show $ evalState (processToken $ words line) (False, [])

-- [Just 2, Just 3]
-- [Either String Float]

type Stack = [Either String Float]
type ProgState = State (Bool, Stack) Stack


-- ["2", "3", "fun"] words "2 3 *"
-- [Just 3, Just 2]

processToken :: [String] -> ProgState
processToken [] = do
  (_, stack) <- get
  return stack

processToken (t:ts) = do
  (ignore, stack) <- get
  if t == "\"" then put (not ignore, stack) >> processToken ts
               else if not ignore then (case t of
                                    "*" -> opMult 
                                    "+" -> opAdd
                                    "pop" -> opPop
                                    _ -> opNum t) >> processToken ts
                else processToken ts


opMult :: ProgState
opMult = do
  (ignore, stack) <- get
  let new_stack = (if length stack < 2 
                    then do
                        Left "Not enough arguments on stack for *" : stack 
                    else do 
                        let a:b:rest = stack
                        ((*) <$> a <*> b) : rest)
  put (ignore, new_stack)
  return new_stack

opAdd :: ProgState
opAdd = do
  (ignore, stack) <- get
  let new_stack = (if length stack < 2 
                      then do
                          Left "Not enough arguments on stack for +" : stack 
                      else do 
                          let a:b:rest = stack
                          ((+) <$> a <*> b) : rest)
  put (ignore, new_stack)
  return new_stack

opPop :: ProgState
opPop = do
    (ignore, stack) <- get
    let new_stack = tail stack
    put (ignore, new_stack)
    return new_stack

opNum :: String -> ProgState
opNum token = do
    (ignore, stack) <- get
    let new_stack = case (readMaybe token :: Maybe Float) of
                        Nothing -> Left ("Parsing error, expected a number, got: " ++ token) : stack
                        Just n -> Right n : stack
    put (ignore, new_stack)
    return new_stack

