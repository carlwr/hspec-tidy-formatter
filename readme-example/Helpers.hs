module Helpers where

import Test.Hspec
import Test.Hspec.Hedgehog
import Control.Concurrent (threadDelay)


it_hh      :: String -> Spec
it_hh_slow :: String -> Spec

it_hh      desc = it desc $ hedgehog $ success
it_hh_slow desc = it desc $ hedgehog $ delayIO

delayIO :: PropertyT IO ()
delayIO = evalIO (threadDelay 6)
