-- | Module for everything to do with the AI logic.
-- Functions for the computer to plan/analyse optimal moves.

module AI( findWinningOrRandomDecision
         ) where

import System.Random (randomRIO)

import Game(Roll(RightRoll,LeftRoll)
           , Move(Move)
           , GameContext
           , rollBoard)

import Board( boardCenterPosition
            , emptyPos
            , placeMarkAt
            , isWin
            , Board
            , Mark
            , swap)

import Control.Monad.State.Lazy (StateT, get, lift, evalStateT)


-- | Helper type for processing possible moves.
-- Given a context of a current player Mark and a Board,
-- the AIState computation computes the optimal move, or,
-- returns nothing if optimal move does not exist.
type AIState = StateT (Mark, Board) Maybe (Int, Maybe Roll)


-- | Computes the next random Move for the current board.
--
-- This is the default baseline implementation.
-- It just picks the roll and index of available positions at random.
-- Note: it can, by chance, pick a winning, or losing move, but it is unlikely.
-- Note: it can only makes legal moves.
findRandomMove :: GameContext -> IO Move
findRandomMove (board, _) = do
    let l = emptyPos board
    -- pick a random position out of all empty ones
    index <- (l !!) <$> randomRIO (0, length l - 1)
    -- just make a random roll or not
    rollGuess <- randomRIO (0::Int, 2)
    return $ Move index (case rollGuess of
                                0 -> Nothing
                                1 -> Just RightRoll
                                _ -> Just LeftRoll)

-- | Computes the next Move for the current board, using
-- simple heuristics: (1) if there is a winning move, take it
-- (2) if there is a winning move for the opponent, take that
-- (3) if boardCenterPosition is free, take it,
-- (4) otherwise, make a random move
--
-- This functions checks if there is a winning move, and
-- if there is, it returns it. Otherwise, it returns a random move as the findMoveDecision above.
findWinningOrRandomDecision :: GameContext -> IO Move
findWinningOrRandomDecision (board, m) = do
  let l = emptyPos board
  randomMove <- findRandomMove (board, m)
  let Move _ randomRoll = randomMove
  let res = case evalStateT (possibleToWin l) (m, board) of
                -- if we can win in the next move, let's win!
                Just (pos, roll) -> Move pos roll
                Nothing -> do
                  case evalStateT (possibleToWin l) (swap m, board) of
                    -- if the opponent can win in the next move, let's prevent it.
                    Just (pos, roll) -> Move pos roll
                    -- otherwise, we take 5 if it is free or just make a random move
                    Nothing -> if 5 `elem ` l
                        then Move boardCenterPosition randomRoll
                        else randomMove
  return res


-- | Helper function to check if it is possible to win from a given
-- move position. The function takes a list of possible positions to check.
{--

Initial implementation. Note the "pyramid of doom" and repetitive pattern
of IF statements. We can refactor it and use FUNCTION COMPOSITION.

possibleToWin :: [Int] -> AIState
-- if we have no moves to make, it is not possible to win
possibleToWin [] = lift Nothing
-- this function takes a list of possible move positions
possibleToWin poss = do
  (m, board) <- get
  let pos = head poss
  let newBoard = placeMarkAt m pos board
  if isWin m newBoard
    then return (pos, Nothing)
    else do
        let newBoardLeft = rotateLeft newBoard
        if isWin m newBoardLeft
            then return (pos, Just Game.LeftRoll)
            else do
                let newBoardRight = rotateRight newBoard
                if isWin m newBoardRight
                    then return (pos, Just Game.RightRoll)
                    else possibleToWin $ tail poss
-}

possibleToWin :: [Int] -> AIState
-- if we have no moves to make, it is not possible to win
possibleToWin [] = lift Nothing
-- this function takes a list of possible move positions
possibleToWin poss = do
    (m, board) <- get
    let pos = head poss
    let checkWin = checkWinWithRoll pos (m, board)
    case checkWin Nothing >>
         checkWin (Just LeftRoll) >>
         checkWin (Just RightRoll) of
           Right _ -> possibleToWin $ tail poss
           Left (pos', roll') -> return (pos', roll')


checkWinWithRoll :: Int -> (Mark, Board) -> Maybe Roll -> Either (Int, Maybe Roll) ()
checkWinWithRoll pos (m, board) roll = do
  let newBoard = rollBoard roll $ placeMarkAt m pos board
  if isWin m newBoard
    then Left (pos, roll)
    else Right ()
