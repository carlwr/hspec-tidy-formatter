{-|
Description : (Internal module)
License     : MIT
-}

{-# LANGUAGE OverloadedStrings #-}

module Test.Hspec.TidyFormatter.Internal
( tidy
) where

import Test.Hspec.TidyFormatter.Internal.Parts

import Test.Hspec.Api.Formatters.V3 qualified as Api
import Test.Hspec.Api.Formatters.V3 (Formatter, FormatM)
import Data.Monoid (Endo (..))
import Control.Monad (when)


--
-- Exported formatter
--

tidy :: Formatter
tidy = Api.Formatter {
  formatterStarted      = pure ()
, formatterDone         = Api.formatterDone Api.checks -- footer, failures
, formatterGroupDone    = \(_ ,_  )     -> nothing
, formatterGroupStarted = \(gs,grp)     -> write     gs (groupStarted grp)
, formatterItemStarted  = \(gs,req)     -> transient gs (itemStarted req)
, formatterItemDone     = \(gs,req) itm -> write     gs (itemDone req itm)
, formatterProgress     = \(gs,_  ) prg -> transient gs (progress prg)
}
  where nothing = pure ()


--
-- hspec API type aliases
--

type Group       = String   -- ([Group],Req) == Api.Path
type Req         = String
type ItemInfo    = String
type Indentation = [Group]  -- [Group] when used to determine indentation


--
-- Chunks and Lines
--

{- |
'Chunks': a sequence of text fragments to be written to the terminal; constitutes a full line or part of a line
'Lines' : a list of 'Chunks' with each element representing one printed line of text, in the String/[String]/lines/unlines sense

Neither ever contains \n-s.

Note: the [Chunks] of 'Lines' is embedded in an outer t'Parts' to allow monadic FormatM conditions to influence whether the newlines implied by [Chunks] are printed or not, i.e. to influence whether a 'Lines' value (including its implied newlines) are printed or not.

> Chunks ~ Silenceable FormatM String
> Lines  ~ Silenceable FormatM [Chunks]

-}

type WithFormat = Endo (FormatM ())

type Chunks = Parts WithFormat String
type Lines  = Parts WithFormat [Chunks]

chunk :: String -> Chunks
chunk = string . filter (/='\n')

line :: Chunks -> Lines
line chunks = value [chunks]


--
-- Output
--

-- `write` and `transient` find and leave the terminal state as: cursor at column 0 of next line to be written

type TransientString = String

write :: Indentation -> Lines -> FormatM ()
write gs = run (run Api.write . vsep . unlines')
  where
    unlines' = foldMap mkLine
    mkLine c = indentation gs <> c <> "\n"

    vsep|isLevel0  = ("\n" <>)
        |otherwise = id

    isLevel0 = null gs

transient :: Indentation -> TransientString -> FormatM ()
transient gs =
  writeTransient
  . (indentationStr gs ++)
  . filter (/='\n')
  where
    writeTransient = whenReportProgress . Api.writeTransient


--
-- Handlers
--

groupStarted :: Group -> Lines
groupStarted group = line (chunk group)

itemStarted :: Req -> TransientString
itemStarted req = "[ ] " ++ req

itemDone :: Req -> Api.Item -> Lines
itemDone req itm =
     line (box <> chunk req <> duration <> ifOneline info)
  <> pending
  <> ifMultiline info
  where
    duration = mkDuration (Api.itemDuration itm)
    info     = mkInfo     (Api.itemInfo     itm)

    (box,pending) =
      case Api.itemResult itm of
        Api.Success     -> (mkBox '✔' 'v' succColor,empty   )
        Api.Failure _ _ -> (mkBox '✘' 'x' failColor,empty   )
        Api.Pending _ s -> (mkBox '‐' '-' pendColor,mkPending s)

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

mkInfo :: ItemInfo -> Info
mkInfo str =
  case lines str of
    []  -> z
    [l] -> z{ ifOneline   =       (one   $ l ) `onlyIf` isVerbose }
    ls  -> z{ ifMultiline = value (multi<$>ls) }
  where
    z       = Info empty empty
    one   s = chunk (" (" <> s <> ")") `with` infoColor
    multi s = chunk ("  " <> s       ) `with` infoColor

mkPending :: Maybe String -> Lines
mkPending mb =
  value $
  extraInd . mapAnn pendColor . chunk <$>
  case lines <$> mb of
    Nothing -> [ "# PENDING"   ]
    Just ls -> ( "# PENDING: "
               , "           " ) `laminate` ls
  where
    laminate (x,y) = zipWith (++) (x : repeat y)
    extraInd c     = "    " <> c

mkDuration :: Api.Seconds -> Chunks
mkDuration (Api.Seconds secs) =
  maybeEmpty (chunk <$> mbStr)
    `with`   infoColor
    `onlyIf` Api.printTimes
  where
    mbStr = case floor (secs * 1000) of
      0  -> Nothing
      ms -> Just $ ("  (" ++ show ms ++ "ms)")

mkBox :: Char -> Char -> Color -> Chunks
mkBox unicode ascii color = "[" <> marker <> "] "
  where
    marker =
      ifThenElse Api.outputUnicode
        (chunk [unicode] `with` color)
        (chunk [ascii  ] `with` color)


--
-- Api shorthands
--

type Color = WithFormat -> WithFormat

infoColor :: Color
pendColor :: Color
succColor :: Color
failColor :: Color

infoColor = (<> Endo Api.withInfoColor   )
pendColor = (<> Endo Api.withPendingColor)
succColor = (<> Endo Api.withSuccessColor)
failColor = (<> Endo Api.withFailColor   )

isVerbose :: FormatM Bool
isVerbose = Api.printTimes
  -- borrow '--times' as verbosity switch since that gives non-verbose by default, which is what we want (using '--expert' would give _verbose_ by default)

whenReportProgress :: FormatM () -> FormatM ()
whenReportProgress = whenM (Api.getConfigValue Api.formatConfigReportProgress)


--
-- Misc
--

indentationStr :: Indentation -> String
indentationStr gs = replicate (length gs * 2) ' '

indentation :: Indentation -> Chunks
indentation gs = chunk (indentationStr gs)


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
