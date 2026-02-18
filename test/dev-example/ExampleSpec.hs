{-# LANGUAGE OverloadedStrings #-}

module ExampleSpec
where

import Test.Hspec
import Test.Hspec.Hedgehog
import Hedgehog.Gen   qualified as Gen
import Hedgehog.Range qualified as Range
import Control.Concurrent
import Control.Monad


spec :: Spec
spec = do
    when True simple
    when True full

simple :: Spec
simple = do
  it_slow "" 200
  it_slow "" 300
  describe "sub1" $ do
    it_slow "1_a" 200
    it_slow "1_b" 300
  it_ok "under root"

full :: Spec
full = describe "FULL" $ do
  it_hh_prog "" NoStats
  it_slow    "1" 100
  it_hh_prog "" NoStats
  it_ok longString
  describe "desc0" $ do
    it_hh      ""
    it_pend    "pending"
    it_hh_prog "" NoStats
    describe "sub" $ do
      it_pendW "sub-pending" "pending msg"
      it_pendW "sub-pending" "pending multi-\nline-\nmsg"
      it_ok    "sub"
  describe "hedgehog prog stats" $ do
    it_hh_prog "" WithStats
    it_ok       "stats-after"
  when False $
    _t_hh_fails "fails"


it_ok       :: String -> Spec
it_pend     :: String -> Spec
it_pendW    :: String -> String -> Spec
it_slow     :: String -> Float -> Spec
it_hh       :: String -> Spec
_t_hh_fails :: String -> Spec
it_hh_prog  :: String -> IsStats -> Spec

it_ok       desc   = it desc $ True
it_pend     desc   = it desc $ pending
it_pendW    desc s = it desc $ pendingWith s
it_slow     desc n = it ("DELAY "   <>desc) $ example  $ delay n
it_hh       desc   = it ("HH "      <>desc) $ hedgehog $ success
_t_hh_fails desc   = it ("HH FAIL " <>desc) $ hedgehog $ prop_fail

data IsStats = WithStats | NoStats deriving (Eq, Show)

it_hh_prog desc isStats = it desc' $ hedgehog $ prop
  where
    desc' = ("HH PROG "<>show isStats<>" "<>desc)
    prop = do
      x <- forAll (Gen.int $ Range.constant 0 20)
      evalIO (delay $ fromIntegral x)
      when (isStats==WithStats) $ do
        classify "  n:   0     " (x== 0         )
        classify "  n:   1- 4  " (x>= 1 && x<= 4)
        classify "  n:   5-19  " (x>= 5 && x<=19)

longString :: String
longString =
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua."

prop_fail :: PropertyT IO ()
prop_fail = do
  i <- forAll $ Gen.int (Range.linear 0 100)
  assert (i <= 10)

delay :: Float -> IO ()
delay n = threadDelay . round $ n * delayScale * 10

delayScale :: Float
delayScale = 15
