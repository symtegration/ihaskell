module IHaskell.Display.SymtegrationSpec (spec) where

import IHaskell.Display
import IHaskell.Display.Symtegration ()
import Symtegration
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "display" $ do
    describe "Expression" $ do
      it "exp 2" $
        display (exp 2 :: Expression)
          `shouldReturn` Display [latex "\\[e^{2}\\]", plain "exp 2"]

    describe "Maybe Expression" $ do
      it "Just (exp 2)" $
        display (Just (exp 2 :: Expression))
          `shouldReturn` Display [latex "\\[e^{2}\\]", plain "exp 2"]

      it "Nothing" $
        display (Nothing :: Maybe Expression)
          `shouldReturn` Display [latex "\\[\\bot\\]", plain "Nothing"]
