module Lib
    ( processLine
    , processTokens
    ) where

import Text.Read (readMaybe)
import Control.Monad.State.Lazy (State, get, put, evalState)




-- | Process a line of input, and produce a string output
--
processLine :: String -> String
processLine line = unwords $ map show $ evalState (processTokens $ words line) (False, [])


-- Our working stack is composed of either floats or errors (as strings)
-- [Either String Float]

type Stack = [Either String Float]
type ProgState = State (Bool, Stack) Stack


-- Process tokens (list of words) and update the program state
processTokens :: [String] -> ProgState
processTokens [] = do
  (_, stack) <- get
  return stack

processTokens (t:ts) = do
  (ignore, stack) <- get
  if t == "\"" then put (not ignore, stack) >> processTokens ts
               else if not ignore then (case t of
                                      "*" -> opMult
                                      "+" -> opAdd
                                      "pop" -> opPop
                                      _ -> opNum t) >> processTokens ts
                                  else processTokens ts


opMult :: ProgState
opMult = do
  (ignore, stack) <- get
  let new_stack = (if length stack < 2 
                    then do
                        Left "Not enough arguments for *" : stack
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
                          Left "Not enough arguments for +" : stack
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




