module Data.MyLogic.EvaluatorSpec
( spec
) where

import Test.Hspec
import Helpers

spec :: Spec
spec = do
  it_hh "is consistent"
  it    "proves this test to fail"
    $ pendingWith "seems to run forever\n->TODO trouble-shoot"
