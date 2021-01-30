--
-- This file demonstrate the use of HUnit tests and QuickCheck property testing.
--

--
-- For the Property Testing we need the following language extension
{-# LANGUAGE TemplateHaskell #-}
-- And for our negative literals this comes really useful
{-# LANGUAGE NegativeLiterals #-}

--
-- You should try to import only things that you use, and try to be specific.
-- This helps in documentation and finding where a given function or type comes from.
--
import Test.DocTest(doctest)
import Test.HUnit(Test(TestCase, TestList, TestLabel), assertEqual, runTestTT)
import Test.QuickCheck(quickCheckAll)
import Test.Hspec(Spec, hspec, describe, shouldBe, it)
import Test.Hspec.QuickCheck(prop)

import Tasks(doubleNum,
             convertWordsIntoInts)

--
-- Unit testing
--
-- Try to select edge cases and representative items for your Unit tests
-- We have identified negative, zero, and positive numbers as such distinct classes
testDoubleNum0 :: Test
testDoubleNum0 = TestCase (assertEqual "Doubling 1 should be 2" 0 (doubleNum 0))
testDoubleNum1 :: Test
testDoubleNum1 = TestCase (assertEqual "Doubling 1 should be 2" 2 (doubleNum 1))
testDoubleNumNeg :: Test
testDoubleNumNeg = TestCase (assertEqual "Doubling -1 should be -2" (-2) (doubleNum -1))

--
-- List of all unit tests
unitTests :: Test
unitTests = TestList [
  TestLabel "doubleNum 1" testDoubleNum1,
  TestLabel "doubleNum 0" testDoubleNum0,
  TestLabel "doubleNum -1" testDoubleNumNeg
  ]

--
-- Property testing
--
-- If it is possible, the best is to define properties of your functions, such that
-- Haskell can generate automatically test cases for you. In our case the invariant
-- is actually the same as the definition of the function, so, it makes little sense,
-- but in other cases you might be able to identify a meaningful properties that you can let Haskell to test
-- automatically.
prop_doubleNum_Doubles :: Int -> Bool
prop_doubleNum_Doubles x = (2 * x) == doubleNum x

-- This little hack with return [] seems to be needed for quickCheckAll to run properly
return []
runPropertyTests :: IO Bool
runPropertyTests = $quickCheckAll


--
-- The same Unit tests and property checks as above, but this time with the use of hspec library
--
spec_doubleNum :: Spec
spec_doubleNum = do
  describe "doubleNum tests" $ do
      it "doubleNum 2 returns 4" $ do
        doubleNum 2 `shouldBe` 4
      it "doubleNum 0 returns 0" $ do
        doubleNum 0 `shouldBe` 0
      it "doubleNum -1 should be -2" $ do
        doubleNum (-1) `shouldBe` (-2)
      prop "doubleNum doubles the number" $ do
        \x -> (2 * x) == doubleNum x

spec_convertLists :: Spec
spec_convertLists = do
  describe "list converstions tests" $ do
    describe "list of Strings into Ints" $ do
      it "converting [\"1\",\"2\"] returns [1,2]" $ do
        convertWordsIntoInts ["1","2"] `shouldBe` [1,2]



-- Main runs all our tests.
-- We use the testing frameworks as libraries, and call them to run our tests.
main :: IO ()
main = do
  -- Let us run first all the doctests from our source files
  doctest ["-isrc", "app/Main.hs"]
  -- Let us run all unit tests
  _ <- runTestTT unitTests
  -- Now let's test all the properties
  _ <- runPropertyTests
  -- Note, you can also run individual property tests like that:
  -- quickCheck prop_doubleNum

  -- Ok, let's try hspec now, and re-run our test logic with this testing framework.
  -- Of course, beneath the hspec library, it still use HUnit and QuickCheck
  hspec $ do
    spec_doubleNum
    spec_convertLists


