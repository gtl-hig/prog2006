module Lib
    ( someFunc,
    replace, toRoman, 
    think, 
    swap, swap3, 
    int3string
    ) where



-- | Think of a number game....
-- think :: (Integral a) => a -> a
--
-- >>> think 21
-- 2
--
-- >>> think 42
-- 2
--
think :: Int -> Int
think x 
    | x < 1 = 2 -- we need to handle the edge case!
    | otherwise = ((((x + 1)^2) - 1) `div` x ) - x

-- | Replace old substring with new in text
--
-- >>> replace "X" "I" "XI"
-- "II"
--
-- >>> replace "X" "I" []
-- ""
-- 
-- >>> replace "IIIII" "V" "IIIIII"
-- "VI"
--
replace :: String -> String -> String -> String
replace _ _ [] = []
replace old new text 
    | length old > length text = text
    | otherwise = 
        if take size text == old
            then new ++ replace old new (drop size text)
            else head text : replace old new (tail text)
        where
            size = length old



-- | converts an int into Roman literal
--
-- >>> toRoman 1
-- "I"
--
-- >>> toRoman 17
-- "XVII"
--
-- >>> toRoman 33
-- "XXXIII"
--
-- >>> toRoman 38
-- "XXXVIII"
--
-- >>> toRoman 4
-- "IV"
--
-- >>> toRoman 9
-- "IX"
--
-- >>> toRoman 8
-- "VIII"
--
toRoman :: Int -> String
toRoman n =
  replace "IIII" "IV" $
  replace "VIIII" "IX" $
  replace "LL" "C" $
  replace "XXXXX" "L" $
  replace "VV" "X" $
  replace "IIIII" "V" $
  replicate n 'I'



-- | Converts a list of numbers into a string row with numbers being comma separated.
--
-- >>> int3string [1,2,3]
-- "1, 2, 3"
--
-- >>> int3string []
-- ""
--
-- >>> int3string [1]
-- ""
--
int3string :: [Int] -> String
int3string xs
    | length xs == 3 = show (head xs) ++ ", " ++
            show (xs !! 1) ++ ", " ++
            show (xs !! 2)
    | otherwise = ""

-- | Swaps element 0th with element 2nd
--
-- >>> swap []
-- []
--
-- >>> swap [1,2]
-- []
--
-- >>> swap [1,2,3]
-- [3,2,1]
--
-- >>> swap [1,2,3,4]
-- []
--
swap :: [Int] -> [Int]
swap xs
    | length xs == 3 = let
           [a, b, c] = xs
        in
           [c, b, a]
    | otherwise = []

-- | alternative swap
--
-- >>> swap3 []
-- []
--
-- >>> swap3 [1,2]
-- []
--
-- >>> swap3 [1,2,3]
-- [3,2,1]
--
-- >>> swap3 [1,2,3,4]
-- []
--
swap3 :: [Int] -> [Int]
swap3 xs
    | length xs /= 3 = []
    | otherwise = [c, b, a] where
      [a, b, c] = xs


-- | This function returs "Hello World"
--
-- >>> someFunc
-- "Hello World"
--
someFunc :: String
someFunc = "Hello World"
