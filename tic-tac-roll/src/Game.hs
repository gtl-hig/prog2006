-- | This module contains all the domain models for the Tic-Tac-Roll Game.
module Game( GameContext
           , Game
           , Move(Move)
           , Roll(LeftRoll,RightRoll)
           , rollBoard
           , GameOutcome(Win,Loss,Draw)
           , Error(TileAlreadyOccupied, PositionOutOfRange)
           , applyMove
           ) where

import Data.Maybe(isNothing)
import Board



-- | The game context represents the actual state of the game,
-- in terms of what the board is and who is making the move.
-- It is simply the current board and the Mark that is to make the move.
type GameContext = (Board, Mark)

-- | There are three possible outcomes of the game.
-- They are captured by the Outcome type.
data GameOutcome = Win Mark | Loss Mark | Draw deriving (Show)

-- | Game represents the game instance, with a current state of the game
-- (modelled as GameContext),and the current game outcome, if it exists.
type Game = (GameContext, Maybe GameOutcome)

-- | There are two possible errors based on the move made by the opponent.
-- Note, that the specification does not use the errors, and we
-- handle them, but do not communicate to the end-user.
data Error = TileAlreadyOccupied | PositionOutOfRange | IllegalMark deriving (Show)


-- | Move represents a legal move. It consist of position
-- (starting from index 1) and possible roll. Note, that move needs to
-- be interpreted in a context og a game context (game tracks who's next move is).
-- In this implementation players cannot make illegal mark in their turn
-- ie. O player cannot make X mark, and vice versa. X player always makes X mark.
-- The mark is always correct and tracked by the game itself.
data Move = Move Int (Maybe Roll) deriving (Eq)

-- | Roll represents the two possible rolls of the board.
-- To handle the "no roll" case, we simply use Maybe Roll type.
-- Nothing will represent "no roll case then.
data Roll = LeftRoll | RightRoll deriving (Eq)

-- | Adding Roll to the Show typeclass.
--
-- >>> show LeftRoll
-- "left"
--
-- >>> show RightRoll
-- "right"
--
instance Show Roll where
    show LeftRoll = "left"
    show RightRoll = "right"


-- | Adding Roll to Read typeclass.
-- Remember, that we have defined our own @show@ also, such that
-- the test rendering of the Rolls is simply @left@ and @right@
--
-- >>> read "left" :: Roll
-- left
--
-- >>> read "right" :: Roll
-- right
--
instance Read Roll where
  readsPrec _ value =
    tryParse [("left", LeftRoll), ("right", RightRoll)]
      where tryParse [] = []    -- If there is nothing left to try, fail
            tryParse ((attempt, result):xs) =
              if take (length attempt) (dropWhile (== ' ') value) == attempt
                then [(result, drop (length attempt) (dropWhile (== ' ')value))]
                else tryParse xs

-- | Point-free definition of what it means to roll a board.
rollBoard :: Maybe Roll -> (Board -> Board)
rollBoard Nothing = id -- we do nothing, for Nothing roll
rollBoard (Just LeftRoll) = rotateLeft
rollBoard (Just RightRoll) = rotateRight



-- | Applies a move to the game state, and checks if the game ends. 
-- Responds with an updated game instance (and appropriate outcome, if game ends)
applyMove :: Move -> GameContext -> Game
applyMove move (board, m) = do
    case validateMove move (board, m) of
        Left _ -> do
            ((board, m), Just (Loss m))
        Right afterValidation -> case applyMoveToGameContext move afterValidation of
            Left _ -> ((board, m), Just (Loss m))
            Right newState -> checkEndOfGameConditions newState



-- =========================
--    Utility functions
-- =========================


-- | Validates a proposed move and conducts sanity checks, if the move is legal.
validateMove :: Move -> GameContext -> Either Error GameContext
validateMove (Move pos _) (board, mark)
    | pos < 1 || pos > lengthBoard board = Left PositionOutOfRange
    | mark == E = Left IllegalMark
    | lookup pos (allPos board) /= Just E = Left TileAlreadyOccupied
    | otherwise = Right (board, mark)


-- | Actually makes a move, that is, apply it to the GameContext.
applyMoveToGameContext :: Move -> GameContext -> Either Error GameContext
applyMoveToGameContext (Move pos roll) (board, mark) = do
  -- prepare new board before roll
  let a_board = placeMarkAt mark pos board
  let new_board = applyRollToBoard roll a_board
  -- all went well, the next player will make the next move
  return (new_board, swap mark)


-- | Checks if the game board indicates End-of-Game.
checkEndOfGameConditions :: GameContext -> Game
checkEndOfGameConditions (board, m)
    | isWin m board = ((board, m), Just (Win m))
    | isWin (swap m) board = ((board, m), Just (Loss m))
    | null (emptyPos board) = ((board, m), Just Draw)
    | otherwise = ((board, m), Nothing)


-- | Applies maybe roll to the board.
applyRollToBoard :: Maybe Roll -> Board -> Board
applyRollToBoard roll board
    | isNothing roll = board
    | roll == Just LeftRoll = rotateLeft board
    | otherwise = rotateRight board
    
