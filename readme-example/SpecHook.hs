module SpecHook
( hook
) where

import qualified Test.Hspec.TidyFormatter as Tidy
import Test.Hspec
import Test.Hspec.Core.Spec (modifyConfig)
import Test.Hspec.Core.Runner (Config(..))
import Test.Hspec.Runner (ColorMode(ColorAlways))

hook :: Spec -> Spec
hook = (setupSpec >>) . Tidy.use

setupSpec :: Spec
setupSpec = modifyConfig $
    (\c -> c { configSeed      = Just seed   })
  . (\c -> c { configColorMode = ColorAlways })
  where
    seed = 1
