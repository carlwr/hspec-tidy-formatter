{-# LANGUAGE MultilineStrings #-}

module Sub.MultilineSpec
( spec
) where

import ExampleSpec hiding (spec)
import Test.Hspec
import Data.Foldable

stringVariants :: [String]
stringVariants =
  [ "word"
  , "\n"
  , "two\nlines"
  , "\nleading newline"
  , "some\n    hanging\n    indentation"
  ]

block :: String -> (String -> Spec) -> Spec
block desc f = do
  describe desc $ do
    it_ok "//-------------------"
    traverse_ f stringVariants
    it_ok "---------------------//\n\n"


spec :: Spec
spec = do
  block "STD"         $ it_ok
  block "SLOW"        $ (\s -> it_slow s 200)
  block "AS-DESCRIBE" $ (\s -> describe s (it_ok "item"))
  block "PENDING"     $ (it "pending" . pendingWith)

  it_ok "\n=====================\n\n"

  it_ok
    """let's try the new
        \"\"\"-multiline
        strings
        \n
    """
  it_ok "...Python-style 🐍\n\n"

  describe "...while we are at it:\n\n\n" $ do
      it_ok
        """
        ---------------------------------------

        📜 SUGGESTIONS TO THE HASKELL COMMITTEE

        ---------------------------------------

        \n\n
        """

      it_ok
        """
        can we have the walrus?:

        ```
        {-# LANGUAGE ImpureBindings #-}             -- new
        {-# LANGUAGE NoReferentialTransparency #-}  -- new

        (:=) :: a -> b -> b
        lhs := rhs  =  unsafeTBD
        infixr 0 :=
        ```

        ref.: u/py1337coder (2010-03-17). 'can this Haskel thing I just tried
              plz remove these annoying *Conflicting definitions* errors I
              keep getting?!', r/haskell.

        \n\n
        """

      it_ok
        """
        change `main :: IO ()` -> `__main__ :: IO ()`

        ref.: Backus, J. & van Rossum, G. (1960-11-01). 'Syntax requiring
              redundant punctuation ceremony made possible by recent trends
              in computer memory sizes', Communications of the ALGOL Committee.

        \n\n
        """

      it_ok
        """
        Proposal: The Global Evaluation Lock (GEL)

        We propose a single global mutex that locks down the entire RTS
        whenever a thunk is evaluated. This simplifies concurrency.

        It would also make stack traces deterministic. Programmers spend a
        non-trivial amount of their time reading runtime exception stack
        traces, studies from other languages have shown.

        ref.: (2007-05-14). 'The future of programming - Why in this year of
              2025 Ruby-On-Rails is the dominating framework and why execution
              efficiency proved to be irrelevant', Viktor Brat (RubyConf 2007).
              https://www.youtube.com/watch?v=8pTEmbeENF4.

        \n\n
        """

      it_ok
        """
        add list comprehensions

        ref.: u/py1337coder (2010-03-19). 'srsly why not just copy python
              on these basic things [0 points, 1 comment]', r/haskell.

        \n\n
        """
