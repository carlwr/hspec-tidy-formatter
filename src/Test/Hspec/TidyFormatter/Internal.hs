{-|
Description : (Internal module)
License     : MIT
-}

{-# LANGUAGE OverloadedStrings #-}

module Test.Hspec.TidyFormatter.Internal
(
  -- * Formatter
  tidy

) where

import Data.Effable

import Test.Hspec.Api.Formatters.V3 qualified as Api
import Test.Hspec.Api.Formatters.V3 (Formatter, FormatM)
import Control.Monad (when)
import Data.String (fromString, IsString)
import Data.List (genericReplicate)
import Data.Functor ((<&>))
import Control.Applicative (Alternative(empty))


--
-- Exported formatter
--

tidy :: Formatter
tidy = Api.Formatter {
  formatterStarted      = nothing
, formatterDone         = Api.formatterDone Api.checks -- footer, failures
, formatterGroupDone    = const nothing
, formatterGroupStarted = \(nst->n,grp)     -> write     n (groupStarted grp)
, formatterItemStarted  = \(nst->n,req)     -> transient n (itemStarted req)
, formatterItemDone     = \(nst->n,req) itm -> write     n (itemDone req itm)
, formatterProgress     = \(nst->n,_  ) prg -> transient n (progress prg)
} where

  nothing = pure ()
  nst     = nesting


--
-- hspec API type aliases
--

type Group         = String   -- ([Group],Req) == Api.Path
type Req           = String
type ItemInfo      = String
type PendingString = String


--
-- Chunks and Lines
--

{- |
'Chunks': a sequence of text fragments to be written to the terminal; constitutes a full line or part of a line
'Lines' : a list of 'Chunks' with each element representing one printed line of text, in the String/[String]/lines/unlines sense

Neither ever contains \n-s.

Note: the [Chunks] of 'Lines' is embedded as well to allow monadic FormatM conditions to influence whether the newlines implied by [Chunks] are printed or not, i.e. to influence whether a 'Lines' value (including its implied newlines) are printed or not.
-}

type Chunks = Effable FormatM String
type Lines  = Effable FormatM [Chunks]

chunk :: String -> Chunks
chunk = string . filter (/='\n')

embedLines :: [Chunks] -> Lines
embedLines = embed

line :: Chunks -> Lines
line chunks = embedLines [chunks]


--
-- Output
--

type TransientString = String

write :: Nesting -> Lines -> FormatM ()
write nst =
  run (run Api.write . vsep . unlines')
  where
    unlines' = foldMap mkLine
    mkLine c = (specIndentation nst <>) (c <> "\n")

    vsep|isLevel0  = ("\n" <>)
        |otherwise = id

    isLevel0 = nst==0

transient :: Nesting -> TransientString -> FormatM ()
transient nst str =
  whenReportProgress $
  Api.writeTransient (specIndentation nst <> str)


--
-- Handlers
--

groupStarted :: Group -> Lines
groupStarted group = line (chunk group)

itemStarted :: Req -> TransientString
itemStarted req = "[ ] " ++ (firstLine req)
  where
    firstLine = concat . take 1 . lines

itemDone :: Req -> Api.Item -> Lines
itemDone req itm =
     line (box <> chunk req <> duration <> infoStr)
  <> pendingBlock
  <> infoBlock
  where
    box                 = "["<>m<>"] "
    duration            = mkDuration      $ Api.itemDuration itm
    (infoStr,infoBlock) = iTuple . mkInfo $ Api.itemInfo     itm
    iTuple info         =  (ifOneline info,ifMultiline info)

    m =
      let pick = ifThenElse Api.outputUnicode in
      case Api.itemResult itm of
        Api.Success     -> succColor (pick "✔" "v")
        Api.Failure _ _ -> failColor (pick "✘" "x")
        Api.Pending _ _ -> pendColor (pick "‐" "-")

    pendingBlock =
      case Api.itemResult itm of
        Api.Pending _ s -> mkPending s
        _               -> empty

progress :: Api.Progress -> TransientString
progress (now,total) = "[" ++ str ++ "]"
  where
    str
      |total==0  = show now
      |otherwise = show now ++ "/" ++ show total


--
-- Handler helpers
--

data Info = Info
  { ifOneline   :: Chunks
  , ifMultiline :: Lines
  }

mapInfoParts :: ApplyWrap -> Info -> Info
mapInfoParts f (Info one multi) = Info (f one) (f multi)

mkInfo :: ItemInfo -> Info
mkInfo str =
  mapInfoParts (unlessExpert . infoColor) $
  case lines str of
    []  -> z
    [l] -> z{ ifOneline   = byAction verbosityM (asStr . chunk $ l) }
    ls  -> z{ ifMultiline = embedLines (asBlock . chunk<$> ls) }
  where
    z       = Info empty empty

    asStr _ Quiet = empty
    asStr s _     = fmtStr s

    asBlock s = fmtBlock s

    fmtStr s = " (" <> s <> ")"
    fmtBlock s = "  " <> s

mkPending :: Maybe PendingString -> Lines
mkPending mb =
  embedLines $
  (extraInd<>) . pendColor . chunk <$>
  case mb of
    Nothing  -> ["# PENDING"]
    Just str ->
      laminate
        "           "
        "# PENDING: "
        str
  where
    extraInd = "    "

mkDuration :: Api.Seconds -> Chunks
mkDuration (Api.Seconds secs) =
  when' Api.printTimes $
  case floor (secs * 1000) of
    0  -> ""
    ms -> infoColor . string $ "  (" ++ show ms ++ "ms)"

{- | Join two columns horizontally.

The first column has @label@ as its first line, and the given padding string as all following lines.

The second column has lines formed by splitting the 'String' argument on '\n's using 'lines'.
-}
laminate :: String -> String -> String -> [String]
laminate pad label body =
  zipWith (++) (label : repeat pad) (lines body)


--
-- Api shorthands
--

type ApplyWrap = ∀ b. Effable FormatM b -> Effable FormatM b


--- Color ---

type Color = ApplyWrap

infoColor :: Color
pendColor :: Color
succColor :: Color
failColor :: Color

infoColor = wrapInside Api.withInfoColor
pendColor = wrapInside Api.withPendingColor
succColor = wrapInside Api.withSuccessColor
failColor = wrapInside Api.withFailColor


--- Verbosity ---

data Verbosity =
    Quiet
  | Verbose
  deriving (Eq, Show, Enum, Bounded)

type VerbosityM = FormatM Verbosity

verbosityM :: VerbosityM
verbosityM = Api.printTimes <&> \case
  False -> Quiet
  True  -> Verbose
  -- borrow '--times' as verbosity switch since that gives non-verbose by default, which is what we want (using '--expert' would give _verbose_ by default)


--- Expert ---

unlessExpert :: ApplyWrap
unlessExpert = wrapInside Api.unlessExpert


--- Progress ---

whenReportProgress :: FormatM () -> FormatM ()
whenReportProgress = whenM (Api.getConfigValue Api.formatConfigReportProgress)


--
-- Spec indentation
--

-- These functions, with signatures that include the 'Nesting' type, are used only for the indentation of /a full spec tree item as a whole/ - indentation of its constituents use other, local, indentation logic.

-- | The nesting depth of a spec tree item.
newtype Nesting = Nesting Int
  deriving (Show, Eq, Ord, Num, Real, Enum, Integral)

nesting :: [Group] -> Nesting
nesting gs = Nesting (2 * length gs)

specIndentation :: IsString m => Nesting -> m
specIndentation n = fromString (genericReplicate n ' ')


--
-- General helpers
--

whenM :: Monad m => m Bool -> m () -> m ()
whenM bM action = do
  b <- bM
  when b action


--
-- Dev notes
--

{- Dev notes:

ref.: source code for built-in formatters:
  https://hackage-content.haskell.org/package/hspec-core/docs/src/Test.Hspec.Core.Formatters.V2.html

---

'writeTransient' of `hspec-api` does roughly:

> writeTransient str = do
>   IO.hPutStr stdout str         -- print payload
>   IO.hFlush  stdout
>   IO.hPutStr stdout "\r\ESC[K"  -- schedule CR, ^K (^K == clear-line)

Effect: the clear-line control sequence will be emitted the next time the output buffer is flushed; until then, the transient payload will be visible in the terminal

---

With this spec tree...

@
spec :: 'Test.Hspec.Spec'
spec = do
  describe "d0" $ do
    describe "d1" $ do
      it "i" $ do
        1 == 1
@

...the t'Api.Path' provided for the inner 'Test.Hspec.it' node will be:

@
path :: 'Api.Path'
path = (["d0","d1"],"i")
@

-}
