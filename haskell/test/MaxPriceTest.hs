import Test.Hspec
import Test.QuickCheck
import MaxPrice
import Control.Exception (evaluate)
import qualified Data.Map as Map

main :: IO () 
main = hspec $

  describe "All Tests" $ do
    it "Canary Test" $ True `shouldBe` True

    it "max-price for zero length" $
      maxPrice 0 (Map.fromList [(1, 1), (2, 2), (3, 3)]) `shouldBe` (0, [[]])

    it "max-price for 1 length" $
      maxPrice 1 (Map.fromList [(1, 1), (2, 2), (3, 3)]) `shouldBe` (1, [[1]])

    it "max-price for 2 length" $
      maxPrice 2 (Map.fromList [(1, 1), (2, 2), (3, 3)]) `shouldBe` (2, [[1, 1], [2]])

    it "max-price for 3 length" $
      maxPrice 3 (Map.fromList [(1, 1), (2, 2), (3, 3)]) `shouldBe` (3, [[1,1,1], [1, 2], [3]])

    it "max-price for 4 length" $
      maxPrice 4 (Map.fromList [(1, 1), (2, 2), (3, 3), (4, 2)]) `shouldBe` (4, [[1,1,1,1], [1,1,2], [1, 3], [2, 2]])

    it "max-price for 5 length" $
      maxPrice 5 (Map.fromList [(1, 1), (2, 2), (3, 3), (4, 2)]) `shouldBe` (5, [[1,1,1,1,1], [1,1,1,2], [1,1,3], [1,2,2], [1,4], [2, 3]])

    it "max-price for 17 length" $
      (length (snd result) == 33) && (fst result == 17)
        where
          result = maxPrice 17 (Map.fromList [(1, 1), (2, 2), (3, 3)])
