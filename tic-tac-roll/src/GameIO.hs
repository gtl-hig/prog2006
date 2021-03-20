module GameIO ( startGame
              , config) where

import Board
import Game
import AI

import Options.Applicative
import System.IO ( IOMode(WriteMode)
                 , Handle
                 , withFile
                 , hPutStrLn
                 , hPutStr
                 , hFlush
                 , stdout)

import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

data Config = Config
  { debug :: String
  , first :: Bool
  }

config :: Parser Config
config = Config
    <$> strOption
        ( long "debug"
        <> short 'd'
        <> metavar "DEBUG"
        <> value "debug.log"
        <> help ( "Where to log debugging output. "
               ++ "Useful for computer vs. computer with pipes. "
               ++ "Default: debug.log"))
    <*> switch
        ( long "first"
        <> short 'f'
        <> help "The computer will play first move")


startGame :: Config -> IO ()
startGame c = withFile (debug c) WriteMode (\handle -> do
    if first c
        then playGame ((newEmptyBoard, O), Nothing) handle
        else playGame ((newEmptyBoard, X), Nothing) handle)

-- | Main entry to the game.
-- As per specification, the first move is played as O player.
-- This implementation uses pattern matching to distinguish
-- between O and X player moves, and, between End-Of-Game case.
--
playGame :: Game -> Handle -> IO ()
playGame (_, Just outcome) h = do
    let gameOverText = processOutcome outcome
    putStrLn gameOverText >> hFlush stdout
    hPutStrLn h gameOverText >> hFlush h
    return ()

playGame ((board, O), Nothing) h = do
    move <- findWinningOrRandomDecision (board, O)
    let (Move index roll) = move
    -- this will print our move to std out
    putStrLn (show index ++ " " ++ maybe " " show roll) >> hFlush stdout
    hPutStrLn h (show index ++ " " ++ maybe " " show roll) >> hFlush h
    let updatedGame = applyMove move (board, O)
    let ((new_board, _), _) = updatedGame
    putStr (showBoardComment new_board) >> hFlush stdout
    hPutStr h (showBoardComment new_board) >> hFlush h
    playGame updatedGame h

playGame ((board, X), Nothing) h = do
    (index', roll) <- processInput
    -- note, that if roll is Nothing due to parsing errors, we ignore it
    let index = fromMaybe 0 index'
    hPutStrLn h (show index ++ " " ++ maybe " " show roll) >> hFlush h
    let updatedGame = applyMove (Move index roll) (board, X)
    let ((board', _), _) = updatedGame
    putStr (showBoardComment board') >> hFlush stdout
    hPutStr h (showBoardComment board') >> hFlush h
    playGame updatedGame h


-- | Obtaining next opponent's move from std input.
-- Minimal validation is done, here:
-- (1) comments and empty lines are ignored
-- (2) position parsing errors are caught
-- (3) roll parsing errors are caught.
-- Note, that roll parsing errors are ignored, and NO ROLL move is done,
-- whereas position parsing errors make the player LOSE.
-- In other words, no error handling beyond catching the errors and returning Nothing.
processInput :: IO (Maybe Int, Maybe Roll)
processInput = do
    line <- getLine
    if not (null line) && (head line /= '#')
        then let 
                items = words line
                index = readMaybe (head items)::Maybe Int
            in
                if length items > 1
                    then return (index, readMaybe (items !! 1)::Maybe Roll)
                    else return (index, Nothing)
        else processInput



-- | Outcome processor.
processOutcome :: GameOutcome -> String
processOutcome (Win m) = gameOverPrefix ++ show m ++ " WON"
processOutcome (Loss m) = gameOverPrefix ++ show m ++ " LOST"
processOutcome Draw = gameOverPrefix ++ "DRAW"

