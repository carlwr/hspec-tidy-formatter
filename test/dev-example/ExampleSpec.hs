{-# LANGUAGE OverloadedStrings #-}

module ExampleSpec
( spec
) where


import Test.Hspec
import Test.Hspec.Hedgehog
import Control.Concurrent
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)


spec :: Spec
spec = do
  when True simple
  when True full

simple :: Spec
simple = do
  it_slow "" 200
  it_slow "" 300
  it_slow "" 600
  describe "sub1" $ do
    it_slow "1_a" 200
    it_slow "1_b" 300
  it_ok "under root"

full :: Spec
full = describe "FULL" $ do
  it_hh_slow  ""   15
  it_slow     "1" 100
  it_slow     "2" 200
  it_slow     "3" 300
  it_hh_cubic ""
  describe "desc0" $ do
    it_hh      ""
    it_pend    "pending"
    it_hh_slow "" 15
    describe "sub" $ do
      it_pendW "sub-pending" "pending msg"
      it_pendW "sub-pending" "pending multi-\nline-\nmsg"
      it_ok    "sub"
  describe "hedgehog stats" $ do
    it_hh_stats "stats"
    it_ok       "stats-after"
  when False $
    _t_hh_fails "fails"


it_ok       :: String -> Spec
it_pend     :: String -> Spec
it_pendW    :: String -> String -> Spec
it_slow     :: String -> Int -> Spec
it_hh       :: String -> Spec
it_hh_slow  :: String -> Int -> Spec
it_hh_cubic :: String -> Spec
_t_hh_fails :: String -> Spec
it_hh_stats :: String -> Spec

it_ok       desc   = it desc $ True
it_pend     desc   = it desc $ pending
it_pendW    desc s = it desc $ pendingWith s
it_slow     desc n = it ("DELAY "   <>desc) $ example  $ delay n
it_hh       desc   = it ("HH "      <>desc) $ hedgehog $ success
_t_hh_fails desc   = it ("HH FAIL " <>desc) $ hedgehog $ prop
it_hh_slow  desc n = it ("HH DELAY "<>desc) $ hedgehog $ evalIO (delay n)
it_hh_cubic desc   = it ("HH CUBIC "<>desc) $ hedgehog $ prop_cubic
it_hh_stats desc   = it ("HH STATS "<>desc) $ hedgehog $ prop_stats

prop_stats :: PropertyT IO ()
prop_stats = do
    x <- forAll (Gen.int $ Range.constant 0 100)
    classify "  n:   0     " (x== 0         )
    classify "  n:   1- 4  " (x>= 1 && x<= 4)
    classify "  n:   5-19  " (x>= 5 && x<=19)
    evalIO (delay 10)

prop_cubic :: PropertyT IO ()
prop_cubic = do
    (x'::Int) <- forAll $ Gen.integral (Range.linear 0 8)
    let x = fromIntegral x'
    liftIO $ threadDelay (round (x ** 3))

prop :: PropertyT IO ()
prop = do
  i <- forAll $ Gen.int (Range.linear 0 100)
  assert (i <= 10)

delay :: Int -> IO ()
delay n = threadDelay $ n * delayScale * 10

delayScale :: Int
delayScale = 15
