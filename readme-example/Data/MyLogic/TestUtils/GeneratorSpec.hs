{-# LANGUAGE OverloadedStrings #-}

module Data.MyLogic.TestUtils.GeneratorSpec
( spec
) where

import Test.Hspec
import Test.Hspec.Hedgehog
import Hedgehog.Gen   qualified as Gen
import Hedgehog.Range qualified as Range

spec :: Spec
spec = do
  it "Hedgehog generator" $ hedgehog $ do
    x <- forAll (Gen.int $ Range.constant 0 100)
    distributions x

distributions :: MonadTest m => Int -> m ()
distributions x = do
  classify "  n:   0     " (x== 0         )
  classify "  n:   1- 4  " (x>= 1 && x<= 4)
  classify "  n:   5-19  " (x>= 5 && x<=19)
  classify "  n:  20-79  " (x>=20 && x<=79)
  classify "  n:  80-    " (x>=80         )
