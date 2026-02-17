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

import Test.Hspec.TidyFormatter.Internal.Parts

import Test.Hspec.Api.Formatters.V3 qualified as Api
import Test.Hspec.Api.Formatters.V3 (Formatter, FormatM)
import Data.Monoid (Endo (..))
import Control.Monad (when)
import Data.Bifunctor
import Data.String (fromString, IsString)
import Data.List (genericReplicate)
import Data.Functor ((<&>))



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

type WithFormat = Endo (FormatM ())

type Chunks' ann = Parts ann String
type Lines'  ann = Parts ann [Chunks]

type Chunks = Chunks' WithFormat
type Lines  = Lines'  WithFormat

chunk :: String -> Chunks
chunk = string . filter (/='\n')

line :: Chunks -> Lines
line chunks = value [chunks]


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
     line (box <> chunk req <> duration <> ifOneline info)
  <> pendingBlock
  <> ifMultiline info
  where
    box                 = "["<>m<>"] "
    duration            = mkDuration      $ Api.itemDuration itm
    info                = mkInfo          $ Api.itemInfo     itm

    m =
      let pick = ifThenElse Api.outputUnicode in
      case Api.itemResult itm of
        Api.Success     -> pick "✔" "v" `with` succColor
        Api.Failure _ _ -> pick "✘" "x" `with` failColor
        Api.Pending _ _ -> pick "‐" "-" `with` pendColor

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

data Info' ann = Info
  { ifOneline   :: Chunks' ann
  , ifMultiline :: Lines'  ann
  }

instance Functor Info' where
  fmap f (Info one multi) = Info (first f one) (first f multi)

type Info = Info' WithFormat

mkInfo :: ItemInfo -> Info
mkInfo str =
  unlessExpert . infoColor <$>
  case lines str of
    []  -> z
    [l] -> z{ ifOneline   = byVerbosity (asStr l) }
    ls  -> z{ ifMultiline = value (asBlock<$>ls) }
  where
    z       = Info empty empty

    asStr _ Quiet = empty
    asStr s _     = chunk $ " (" <> s <> ")"
    asBlock s = chunk $ "  " <> s

mkPending :: Maybe PendingString -> Lines
mkPending mb =
  value $
  (extraInd<>) . mapAnn pendColor . chunk <$>
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
  maybeEmpty (chunk <$> mbStr)
    `with`   infoColor
  where
    mbStr = case floor (secs * 1000) of
      0  -> Nothing
      ms -> Just $ ("  (" ++ show ms ++ "ms)")

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


--- Color ---

type Color = WithFormat -> WithFormat

infoColor :: Color
pendColor :: Color
succColor :: Color
failColor :: Color

infoColor = (<> Endo Api.withInfoColor   )
pendColor = (<> Endo Api.withPendingColor)
succColor = (<> Endo Api.withSuccessColor)
failColor = (<> Endo Api.withFailColor   )


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

byVerbosity :: (Verbosity -> Parts WithFormat b) -> Parts WithFormat b
byVerbosity = byAction verbosityM


--- Expert ---

unlessExpert :: WithFormat -> WithFormat
unlessExpert = (<> Endo Api.unlessExpert)


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
