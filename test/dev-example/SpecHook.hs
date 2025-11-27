module SpecHook
( hook
) where

import qualified Test.Hspec.TidyFormatter as Formatter
import Test.Hspec
import Test.Hspec.Core.Spec (modifyConfig)
import Test.Hspec.Core.Runner (Config(..))
import Test.Hspec.Runner (ColorMode(ColorAlways))

hook :: Spec -> Spec
hook = (setupSpec >>) . Formatter.use -- . parallel


setupSpec :: Spec
setupSpec = modifyConfig $ \c -> c
  { configSeed      = Just seed
  , configColorMode = ColorAlways
  }
  where
    seed = 1
