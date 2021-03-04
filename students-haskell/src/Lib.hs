module Lib
    ( execCmd
    ) where

import Data.Char (isUpper, isLetter)
import Text.Read (readMaybe)
import System.Exit (exitSuccess)
import Data.Validation

--
-- STUDENT
--
type Name = String
type Surname = String
type Age = Int

data Student = Student {  name    :: Name
                        , surname :: Surname
                        , age     :: Age
                        } deriving (Eq, Show)

type StudentDB = [Student]

-- | Adds new student to the db
addStudent :: StudentDB -> Student -> StudentDB
addStudent db stud = stud:db

--------------------------
-- RULES FOR VALIDATION
--------------------------

stringCannotBeEmpty :: String -> String -> Validation [String] String
stringCannotBeEmpty prefix [] = Failure [ prefix ++ " cannot be empty"]
stringCannotBeEmpty _ t = Success t

stringLength :: Int -> String -> String -> Validation [String] String
stringLength textLength prefix text 
   | length text < textLength = Failure [ prefix ++ " cannot be shorter than " ++ show textLength]
   | otherwise = Success text

stringFirstCap :: String -> String -> Validation [String] String
stringFirstCap prefix text@(t:_)
   | isUpper t = Success text
   | otherwise = Failure [prefix ++ " must have first letter Capitalized"]

stringOnlyLetters :: String -> String -> Validation [String] String
stringOnlyLetters prefix text
   | all isLetter text = Success text
   | otherwise = Failure [prefix ++ " should contain ONLY letters"]

-----------------------------

validateName :: Name -> Validation [String] Name 
validateName text = case sequenceA $ [stringCannotBeEmpty, stringOnlyLetters, stringFirstCap, (stringLength 2)] 
                       <*> ["Name"]
                       <*> pure text of
                           Success side -> Success $ head side
                           Failure f -> Failure f


validateSurname :: String -> Validation [String] Surname
validateSurname text = case sequenceA $ [stringCannotBeEmpty, stringOnlyLetters, stringFirstCap, (stringLength 4)] 
                       <*> ["Surname"]
                       <*> pure text of
                           Success side -> Success $ head side
                           Failure f -> Failure f

validateAge :: String -> Validation [String] Age
validateAge [] = Failure ["empty Age"]
validateAge ts = case readMaybe ts :: Maybe Int of
    Just num -> if num >= 18 && num <= 130 then 
      Success num else Failure ["Age must be between 18 and 130"]
    Nothing -> Failure ["error parsing Age"]


-- | Command processor
execCmd :: StudentDB -> IO StudentDB
execCmd db = do
    line <- getLine
    if null line then execCmd db else do
        let cmd = words line
        case head cmd of
            "new"   -> do
                let stud = createStudent $ tail cmd
                case stud of
                    Success s -> execCmd (addStudent db s)
                    Failure err  -> do
                        print err
                        execCmd db
            "list"  -> mapM_ print db >> execCmd db
            "end"   -> exitSuccess
            _       -> do
                putStrLn "unknown command"
                execCmd db
                

-- | Creates an instance of a student from raw String data, or fails with an error
createStudent:: [String] -> Validation [String] Student
createStudent ws
  | length ws /= 3 = Failure ["should provide 3 arguments: Name Surname Age"]
  | otherwise = Student <$> vName <*> vSurname <*> vAge where
    [n,s,a] = ws
    vName = validateName n
    vSurname = validateSurname s
    vAge = validateAge a