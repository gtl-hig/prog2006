-- | This module contains all the fundamental data types 
-- and domain models for modelling the game Board.
--
module Board ( size, sizeRow, boardCenterPosition
             , Mark(X, O, E)
             , swap
             , Board(Board)
             , newEmptyBoard, allPos, emptyPos
             , showBoard, showBoardComment
             , gameOverPrefix
             , rotateLeft, rotateRight
             , isWin
             , placeMarkAt
             , lengthBoard
             ) where

import Data.List(transpose)


-- =============
--   CONSTANTS
-- =============

-- | Constant defining the size of the board's row.
-- For 3x3 board it should by 3.
sizeRow :: Int
sizeRow = 3
-- | Constant defining the size of the board.
-- For 3x3 board it should by 9.
size :: Int
size = sizeRow * sizeRow
-- | Constant defining the center position on the board
-- For 3x3 board that should be 5.
boardCenterPosition :: Int
boardCenterPosition = 5

-- | Constant defining the prefix for comments
-- As per task specification.
commentPrefix :: String
commentPrefix = "# "

-- | Constant defining the prefix for game over text.
-- As per task specification.
gameOverPrefix :: String
gameOverPrefix = "GAME OVER "



-- =============
--     PUBLIC
-- =============


-- | A player can mark a tile using either an @X@ or an @O@ symbols.
-- An empty Mark, @E@ should be visually represented as underscore @_@
data Mark = X | O | E deriving (Eq)

-- | Making Mark an Instance of Show
--
-- >>> show X
-- "X"
--
-- >>> show O
-- "O"
--
-- >>> show E
-- "_"
--
instance Show Mark where
  show X = "X"
  show O = "O"
  show E = "_"

-- Alternatively, you can just define a function
-- for converting your custom data type to String
-- mark2String :: Mark -> String
-- mark2String X = "X"
-- mark2String O = "O"
-- mark2String E = "_"


-- | Switches one mark for the other.
-- Note: in tests, we have to use what @show Mark@ will produce,
-- which in our case is not @(E,E)@, but @(_,_)@
--
-- >>> swap O
-- X
--
-- >>> swap X
-- O
--
-- >>> swap E
-- _
swap :: Mark -> Mark
swap X = O
swap O = X
swap E = E


-- | Representation for the Board via a custom data type
-- The board is a list of Marks.
newtype Board = Board [Mark] deriving (Eq, Show)


-- | Returns an empty board.
--
-- >>> newEmptyBoard
-- Board [_,_,_,_,_,_,_,_,_]
newEmptyBoard :: Board
newEmptyBoard = Board $ replicate size E


-- | Turns the Board int an ordered position list.
-- Returns a list of tuples with 1-index based positions and their marks.
--
-- >>> allPos (Board [X,X,O,E])
-- [(1,X),(2,X),(3,O),(4,_)]
--
allPos :: Board -> [(Int,Mark)]
allPos (Board []) = []
allPos (Board ms) = zip [1..] ms


-- | Returns positions (indices counting from 1) of empty positions on a given Board.
-- 
-- >>> emptyPos (Board [X,X,O,E])
-- [4]
-- 
-- >>> emptyPos (Board [X,E,O,E,X,E,X,O])
-- [2,4,6]
--
-- >>> emptyPos newEmptyBoard
-- [1,2,3,4,5,6,7,8,9]
--
emptyPos :: Board -> [Int]
emptyPos (Board []) = []
emptyPos xs = [ i | (i, m) <- allPos xs, m == E]


-- | Places a specific mark on a specific position.
--
-- >>> placeMarkAt X 1 (Board [O,X,O])
-- Board [X,X,O]
--
-- >>> placeMarkAt X 0 (Board [O,X,O])
-- Board []
--
-- >>> placeMarkAt X 3 (Board [O,X,O,X])
-- Board [O,X,X,X]
--
placeMarkAt :: Mark -> Int -> Board -> Board
placeMarkAt mark pos (Board xs)
  | pos > length xs || pos < 1 = Board []
  | otherwise = Board (start ++ [mark] ++ end) where
    (a,b) = splitAt (pos - 1) xs
    start = a
    end = drop 1 b

-- | Returns a String representation of the board, as single line
-- Note, this is handy for debugging and internal representation for parsing, etc.
-- For comments and human readable representation, use showBoardComment
--
-- >>> showBoard $ Board [E,X,O]
-- "_ X O"
--
-- >>> showBoard (Board [X,X,X,O,E,E])
-- "X X X O _ _"
--
showBoard :: Board -> String
showBoard (Board []) = ""
showBoard (Board [x]) = show x
showBoard (Board (x:xs)) = show x ++ " " ++ showBoard (Board xs)


-- | Returns a comment representation of the board. The board
-- String is split into 3 lines, and each line is prefixed with "# "
-- 
-- >>> showBoardComment (Board [X,X,O])
-- "# X X O\n"
--
-- -- >>> showBoardComment (Board [X,X,O,X,X,X,O])
-- "# X X O\n# X X X\n# O\n"
--
-- >>> showBoardComment newEmptyBoard
-- "# _ _ _\n# _ _ _\n# _ _ _\n"
--
showBoardComment :: Board -> String
showBoardComment (Board ms) = 
    let
        boardAsRows = groupBySizeRow ms
        boardAsLines = foldl (\acc g -> acc ++ [commentPrefix ++ showBoard (Board g)]) [] boardAsRows
    in
        unlines boardAsLines
  

-- | Rotates the board left, as per specification, with swapping.
-- The rotation keeps the upper left corner of the board intact,
-- and rotates the right upper corner sort of "twice",
-- jumping over the left upper corner.
-- If the board has illegal dimensions, an empty board is returned.
--
-- >>> rotateLeft (Board [X,X,O,O,O,O,X,O,X])
-- Board [X,O,X,X,O,O,O,O,X]
rotateLeft :: Board -> Board
rotateLeft (Board ms) = Board $ rotateMarksLeft $ swapTopCorners ms

-- | Rotates the board right, as per specification, with swapping.
-- The rotation keeps the upper right corner of the board intact,
-- and rotates the left upper corner sort of "twice",
-- jumping over the right upper corner.
-- If the board has illegal dimensions, an empty board is returned.
--
-- >>> rotateRight (Board [X,X,O,O,O,O,X,O,X])
-- Board [X,O,O,O,O,X,X,O,X]
rotateRight :: Board -> Board
rotateRight (Board ms) = Board $ rotateMarksRight $ swapTopCorners ms

lengthBoard :: Board -> Int
lengthBoard (Board a) = length a



-- | Checks if a given mark has connect 3 winning position.
--
-- >>> isWin O (Board [X,X,X,O,O,O,X,O,X])
-- True
--
-- >>> isWin X (Board [X,X,X,O,O,O,X,O,X])
-- True
--
-- >>> isWin O (Board [O,X,X,O,O,X,X,O,O])
-- True
--
isWin :: Mark -> Board -> Bool
isWin m (Board board) = (m, m, m) `elem` winPositions board




-- ==============================
--   Module internal functions
-- ==============================


-- | Utility methods for rotating the board. Should not be exposed outside module.
-- We assume errors are handled elsewhere and here we going to get
-- a correctly sized list for the board, otherwise the method returns an empty list.
rotateMarksLeft :: [Mark] -> [Mark]
rotateMarksLeft ms
    | length ms /= size = []
    | otherwise = concat . reverse . transpose . groupBySizeRow $ ms

rotateMarksRight :: [Mark] -> [Mark]
rotateMarksRight ms
    | length ms /= size = []
    | otherwise = concat . transpose . reverse . groupBySizeRow $ ms


-- | Swaps the top corners of the 3x3 board.
-- If the board is does not have proper size, empty list is returned.
--
-- swapTopCorners [X,X,X,O,O,O]
-- []
--
-- >>> swapTopCorners [X,X,O,O,O,O,E,E,E]
-- [O,X,X,O,O,O,_,_,_]
--
swapTopCorners :: [Mark] -> [Mark]
swapTopCorners ms
    | length ms /= size = []
    | otherwise = ms !! (sizeRow -1) : ms !! 1 : head ms : drop sizeRow ms


-- | Utility function that groups elements of a list into a list of 3 elements.
--
-- >>> groupBySizeRow [1,2,3,4,5,6,7,8,9]
-- [[1,2,3],[4,5,6],[7,8,9]]
--
-- >>> groupBySizeRow [1,2,3,4]
-- [[1,2,3],[4]]
--
groupBySizeRow :: [a] -> [[a]]
groupBySizeRow [] = []
groupBySizeRow xs = take sizeRow xs : groupBySizeRow (drop sizeRow xs)

-- | This utility function only works for 3x3 boards.
-- TODO rewrite it such that it will work for arbitrary board sizes.
winPositions :: [Mark] -> [(Mark, Mark, Mark)]
winPositions [a, b, c, d, e, f, g, h, i] =
  [ (a, b, c)
  , (d, e, f)
  , (g, h, i)
  , (a, d, g)
  , (b, e, h)
  , (c, f, i)
  , (a, e, i)
  , (g, e, c)
  ]