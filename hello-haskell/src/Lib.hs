module Lib
    (
    helloWorld,
    askForNumberAndDoubleIt
    ) where

  -- We need that for manipulating Buffering of stdout
  import System.IO(hSetBuffering, BufferMode(NoBuffering), stdout)
  -- We need to import our pure functions here
  import Tasks ( doubleNum )

  -- | helloWorld prints Hello World to the screen.
  --
  -- Examples
  -- 
  -- >>> helloWorld
  -- Hello World
  --
  helloWorld :: IO ()
  helloWorld = putStrLn "Hello World"

  -- | askForNumberAndDoubleIt asks for ...
  --
  -- We keep it here as it uses IO. Our function makes use of other pure functions.
  askForNumberAndDoubleIt :: IO ()
  askForNumberAndDoubleIt = do
    -- One way of forcing flush on the stdout
    hSetBuffering stdout NoBuffering
    putStr "Please give me a number: "
    -- Another way of forcing the flush
    -- hFlush stdout
    numberLine <- getLine
    putStr "The number doubled is: "
    print . doubleNum . read . head . words $ numberLine