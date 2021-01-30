-- | Module that contains our tasks. Keeping it PURE.
module Tasks(doubleNum,
             convertWordsIntoInts) where

-- | Doubles the given number.
--
-- Examples
--
-- >>> doubleNum 3
-- 6
--
-- >>> doubleNum 45
-- 90
--
doubleNum :: Int -> Int
doubleNum x = x + x

-- | Converts a list of Strings into a list of Ints
--
-- Examples
--
-- >>> convertWordsIntoInts ["1", "2", "3"]
-- [1,2,3]
--
-- >>> convertWordsIntoInts []
-- []
--
-- >>> convertWordsIntoInts ["1", "-2", "3", "-4", "0"]
-- [1,-2,3,-4,0]
convertWordsIntoInts :: [String] -> [Int]
convertWordsIntoInts = map (read :: String -> Int)

