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
import Data.List (find)
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
-- `Chunks`
--

type WithFormat = Endo (FormatM ())

{- | A sequence of text fragments to be written to the terminal

Chunks:    full line or part thereof
[Chunks]:  lines, in the String/[String]/lines/unlines sense

Neither ever contains \n-s.

> Chunks ~ Silenceable FormatM String
-}
type Chunks = Parts WithFormat String

chunk :: String -> Chunks
chunk = string . filter (/='\n')


--
-- Output
--

-- `write` and `transient` find and leave the terminal state as: cursor at column 0 of next line to be written

type TransientString = String

write :: Indentation -> [Chunks] -> FormatM ()
write gs = run Api.write . vsep . unlines'
  where
    unlines' = foldMap mkLine
    mkLine c = indentation gs <> c <> "\n"

    vsep|isLevel0  = ("\n" <>)
        |otherwise = id

    isLevel0 = null gs

transient :: Indentation -> TransientString -> FormatM ()
transient gs =
  whenM (Api.getConfigValue Api.formatConfigReportProgress)
  . Api.writeTransient
  . (indentationStr gs ++)
  . filter (/='\n')


--
-- Handlers
--

groupStarted :: Group -> [Chunks]
groupStarted group = [chunk group]

itemStarted :: Req -> TransientString
itemStarted req = "[ ] " ++ req

itemDone :: Req -> Api.Item -> [Chunks]
itemDone req itm =
  [ box <> chunk req <> duration <> ifOneline info  ]
  ++
  firstNonEmptyOf
    [ pending
    , ifMultiline info
    ]
  where
    duration = mkDuration (Api.itemDuration itm)
    info     = mkInfo     (Api.itemInfo     itm)

    (box,pending) =
      case Api.itemResult itm of
        Api.Success     -> (mkBox '✔' 'v' succColor,[]   )
        Api.Failure _ _ -> (mkBox '✘' 'x' failColor,[]   )
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

data InfoChunks = InfoChunks
  { ifOneline   ::  Chunks
  , ifMultiline :: [Chunks]
  }

mkInfo :: ItemInfo -> InfoChunks
mkInfo str =
  let z = InfoChunks empty []
  in
    case lines str of
      []  -> z
      [l] -> z{ ifOneline   = fmtOne l `onlyIf` isVerbose }
      ls  -> z{ ifMultiline = fmtMulti <$> ls }
  where
    fmtOne   s = chunk (" (" <> s <> ")") `with` (<>infoColor)
    fmtMulti s = chunk ("  " <> s       ) `with` (<>infoColor)

mkPending :: Maybe String -> [Chunks]
mkPending mb =
  extraInd . mapAnn (<> pendColor) . chunk <$>
  case lines <$> mb of
    Nothing -> [ "# PENDING"   ]
    Just ls -> ( "# PENDING: "
               , "           " ) `laminate` ls
  where
    laminate (x,y) = zipWith (++) (x : repeat y)
    extraInd c     = "    " <> c

mkDuration :: Api.Seconds -> Chunks
mkDuration (Api.Seconds secs) =
  fromMaybe (chunk <$> mbStr)
    `with`   (<> infoColor)
    `onlyIf` Api.printTimes
  where
    mbStr = case floor (secs * 1000) of
      0  -> Nothing
      ms -> Just $ ("  (" ++ show ms ++ "ms)")

mkBox :: Char -> Char -> WithFormat -> Chunks
mkBox unicode ascii f = "[" <> marker <> "] "
  where
    marker =
      ifThenElse Api.outputUnicode
        (chunk [unicode] `with` (<>f))
        (chunk [ascii  ] `with` (<>f))

infoColor :: WithFormat
pendColor :: WithFormat
succColor :: WithFormat
failColor :: WithFormat

infoColor = Endo Api.withInfoColor
pendColor = Endo Api.withPendingColor
succColor = Endo Api.withSuccessColor
failColor = Endo Api.withFailColor

isVerbose :: FormatM Bool
isVerbose = Api.printTimes
  -- borrow '--times' as verbosity switch since that gives non-verbose by default, which is what we want (using '--expert' would give _verbose_ by default)


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

firstNonEmptyOf :: [[a]] -> [a]
firstNonEmptyOf xss =
  case find (not . null) xss of
    Just xs -> xs
    _       -> []


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
