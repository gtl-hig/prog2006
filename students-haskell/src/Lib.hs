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

validateName :: String -> Validation [String] Name 
validateName [] = Failure ["empty Name"]
validateName text@(t:_)
   | length text < 2 = Failure ["invalid Name: must be at least 2 characters"]
   | not $ isUpper t = Failure ["invalid Name: first letter must be Capitalized"]
   | not $ all isLetter text = Failure ["invalid Name: only letters allowed"]
   | otherwise = Success text

validateSurname :: String -> Validation [String] Surname
validateSurname [] = Failure ["empty Surname"]
validateSurname text@(t:_) 
   | length text < 4 = Failure ["invalid Surname: must be at least 4 characters"]
   | not $ isUpper t = Failure ["invalid Surname: first letter must be Capitalized"]
   | not $ all isLetter text = Failure ["invalid Surname: only letters allowed"]
   | otherwise = Success text

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