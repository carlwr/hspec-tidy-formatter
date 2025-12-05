module Data.MyLogic.Core.InstancesSpec
( spec
) where

import Test.Hspec
import Helpers

spec :: Spec
spec = do
  it "Functor"   True
  it "Monoid" True
  describe "is Applicative-lawful per" $ do
    it_hh_slow "Identity"
    it_hh      "Homomorphism"
    it_hh      "Interchange"
    it_hh      "Composition"

