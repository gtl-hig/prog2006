
import Test.DocTest(doctest)


main :: IO ()
main = do
  -- Let us run first all the doctests from our source files
  doctest ["-isrc", "app/Main.hs"]
