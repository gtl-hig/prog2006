-- Our main entry point lives here, ie. the module Main with the `main :: IO ()` function
-- Module name is followed by all exported functions. GHC would work and compile the
-- executable even if the line `module Main(main) where` was skipped it would add it implicitly
module Main(main) where
-- Note also, that you should be specific and say Main(main) as it allows the compiler
-- to inline any other functions that you might have defined here. Given that they are NOT
-- exported, they can be inlined by the compiler.

-- We have to import our Lib module to get access to the functions defined there
import Lib

-- | Entry point. Try to keep it simple.
-- Try to isolate IO code into its own dedicated module.
main :: IO ()
main = do
  helloWorld
  askForNumberAndDoubleIt
