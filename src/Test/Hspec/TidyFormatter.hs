{-|
Description : A custom hspec formatter for easy-to-read terminal output.
License     : MIT
-}

module Test.Hspec.TidyFormatter
(
  -- * Formatter
  use
, register
, formatter

  -- * Re-exported API
, module Test.Hspec.Api.Formatters.V3

) where


import Test.Hspec.TidyFormatter.Internal qualified as Internal
import Test.Hspec.Api.Formatters.V3


-- | Use the formatter by default. Also makes it available for selection with @hspec --format=tidy@.
use :: SpecWith () -> SpecWith ()
use = (modifyConfig (useFormatter formatter) >>)

-- | Make the formatter available for selection with @hspec --format=tidy@.
register :: SpecWith a -> SpecWith a
register = (modifyConfig (registerFormatter formatter) >>)

-- | The named formatter.
formatter :: (String,Formatter)
formatter = (name,Internal.tidy)


--
-- non-exported
--

name :: String
name = "tidy"
