
import Test.DocTest(doctest)
import Test.QuickCheck(property)
import Test.Hspec(Spec, hspec, describe, shouldBe, it)

-- We need to import all our functions for tests
import Lib

-- | Unit and property tests for Lecture Three
testLectureThree :: Spec
testLectureThree =
    describe "Various tests for Lecture Three" $ do
      describe "swap3, for swapping first and third list element " $ do
        it "works for [1, 2, 3]" $ do
            swap3 [1, 2, 3] `shouldBe` [3, 2, 1]
        it "handles edge case with [1,2]" $ do
            swap3 [1, 2] `shouldBe` []
            
      describe "replace" $ do
        it "works for 'IIIII' 'V' 'IIIIII' -- 'VI' " $ do
            replace "IIIII" "V" "IIIIII" `shouldBe` "VI"
            
            
      describe "think of a number" $ do
        it "works as expected and always guesses 2!" $
            property $ \x -> think x == 2



main :: IO ()
main = do 
  -- Let us run first all the doctests from our source files
  doctest ["-isrc", "app/Main.hs"]
  hspec $ do 
    testLectureThree
