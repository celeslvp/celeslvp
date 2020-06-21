import PdePreludat
import Library
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Tests  Cálculo de precio" $ do
    it "test básico" $ do
      1 + 1 `shouldBe` 2