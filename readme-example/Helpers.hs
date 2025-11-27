module Helpers where

import Test.Hspec
import Test.Hspec.Hedgehog


it_hh :: String -> Spec
it_hh desc = it desc $ hedgehog $ success
