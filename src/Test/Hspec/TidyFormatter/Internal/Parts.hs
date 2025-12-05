{-|
Description : Ordered annotated sequences useful for conditional emission
License     : MIT

/The mise-en-place utility set needed for a readable, declarative implementation of "Test.Hspec.TidyFormatter" -- in the dress of a small, general annotated-sequence module./

The t'Parts' type expresses /ordered annotated sequences/. It is expected to be useful primarily in its 'Silenceable' specialization.

The 'Silenceable' type, together with utility functions and instances, can be useful as an abstraction over sequences of /pairs of pure values and effect-modifying functions/.

-}


{-# LANGUAGE OverloadedStrings #-}

module Test.Hspec.TidyFormatter.Internal.Parts
where

import Control.Monad (when)
import Data.String (IsString (..))
import Data.Monoid (Endo (..))


{- $setup
>>> import Data.Monoid (Sum)
-}

-- * Type

{- | An ordered sequence of /elements/ where each /element/ consists of an /annotation/ and a /label/.

This type is a thin newtype wrapper over [(a,b)], and usefully different from that bare type in nuance only: in t'Parts', the first tuple component (the /annotation/) is assumed to be meaningful only together with the second tuple component (the /label/). Therefore, t'Parts' have no utility functions or instances that allow combining over only the annotations of a t'Parts' - hence the lack of (Bi)Foldable and (Bi)Traversable. t'Parts' instead offers up to (Bi)Functor, Semigroup and Monoid; all of which retains the structural pairing of annotations and labels. For combining over the @(ann,b)@ pairs (elements) of a t'Parts', 'foldParts' and 'interpret' are provided instead.

Further, the justification of a 'Parts' type could be claimed to rely solely on its specialization to the 'Silenceable' type.
-}
newtype Parts ann b = Parts [(ann,b)]
  deriving (Functor, Read, Show, Eq, Ord)

instance Semigroup (Parts ann b) where
  Parts xs <> Parts ys = Parts (xs ++ ys)

instance Monoid (Parts ann b) where
  mempty = Parts []

instance (Monoid ann, IsString b) => IsString (Parts ann b) where
  fromString = string


-- * Create

{- $create

Examples:

>>> singleton [] "a" :: Parts [Int] String
Parts [([],"a")]

>>> string "a" :: Parts [Int] String
Parts [([],"a")]

>>> p = singleton [] "a" :: Parts [Int] String
>>> :seti -XOverloadedStrings
>>> p <> "b"
Parts [([],"a"),([],"b")]
-}

singleton :: ann -> b -> Parts ann b
singleton ann x = Parts [(ann,x)]

-- | Embed a value annotated with 'mempty'.
value :: Monoid ann => b -> Parts ann b
value x = Parts [(mempty,x)]

-- | Embed a string literal annotated with 'mempty'.
string :: (Monoid ann, IsString b) => String -> Parts ann b
string s = Parts [(mempty,fromString s)]

empty :: Parts ann b
empty = Parts []


-- * Modify

maybeEmpty :: Maybe (Parts ann b) -> Parts ann b
maybeEmpty = \case
  Just p -> p
  _      -> empty

{- | Map annotations.

@
mapAnn == 'first'
@
-}
mapAnn :: (ann->ann') -> Parts ann b -> Parts ann' b
mapAnn f (Parts xs) = Parts (f' <$> xs)
  where
    f' (ann,b) = (f ann,b)

{- | Flipped 'mapAnn'.

Examples:

>>> p = string "ab" :: Parts [Int] String
>>> p
Parts [([],"ab")]

>>> p `with` (++[1])
Parts [([1],"ab")]

The high precedence of the operator variant means it binds tighter than e.g. '<>', which is inteded to facilitate constructs such as:

>>> :seti -XOverloadedStrings
>>> :{
let parts :: Parts (Sum Int) String
    parts =    "a" `with` (+1)
            <> "b" `with` (+2)
in  parts
:}
Parts [(Sum {getSum = 1},"a"),(Sum {getSum = 2},"b")]

(Note: above, the 'IsString' instance promotes the string literals to 'Parts', initializing the annotation to 'mempty' == 'Sum 0'.)
-}
with :: Parts ann b -> (ann -> ann') -> Parts ann' b
with = flip mapAnn
infixl 7 `with`


-- * Fold

{- | Right-fold a t'Parts'.

@
v'Parts' . 'foldParts' (:) [] == 'id'
@
-}
foldParts ::
     ((ann,b) -> acc -> acc) -- ^ combine
  -> acc                     -- ^ initial aggregate
  -> Parts ann b
  -> acc
foldParts f z (Parts xs) = foldr f z xs


-- * Interpret

{- | Interpret an annotated sequence by applying a function to each element and combine the results.

@
'interpret' (,)                == 'foldParts'
'Parts' . 'interpret' (,) (:) [] == 'id'
@

Examples:

>>> parts = singleton 'a' 1 <> singleton 'b' 2
>>> interpret (,) (:) [] parts
[('a',1),('b',2)]

> >>> :seti -XOverloadedStrings
> >>> import Data.Monoid (Endo(..))
> >>> interp  = interpret (\ann -> appEndo ann . putStr) (>>) (pure ())
> >>> bold    = putStr "\ESC[1m"
> >>> stop    = putStr "\ESC[0m"
> >>> asBold  = Endo $ \x -> bold >> x >> stop
> >>> interp $ "plain, " <> "bold" `with` (<> asBold)
> <prints "plain, bold" with "bold" bold-formatted>
-}
interpret :: ∀ ann b c acc.
     (ann->b->c)   -- ^ interpreting one element
  -> (c->acc->acc) -- ^ adding an interpretation to the aggregate
  -> acc           -- ^ initial aggregate
  -> Parts ann b   -- ^ the t'Parts' to interpret
  -> acc           -- ^ returned interpretation
interpret interp f = foldParts f'
  where
    f' :: (ann,b) -> acc -> acc
    f' (ann,b) acc = interp ann b `f` acc


-- * t'Parts' with 'Silenceable' elements

{- $silenceable

The 'Silenceable' specialization of t'Parts' have annotations that /describe how to transform the effect of emitting the label it is paired with/. This allows embedding per-element effectfully-predicated include/suppress decisions in the annotations. The decisions are effectuated at interpretation time.
-}

type Silenceable m b = Parts (Endo (m ())) b


-- ** Conditionals

{- $silenceable-conditionals

These transformations are semantically meaningful, and in a way that aligns with the function names, if later interpreted with 'run', e.g. @'run' 'putStr'@.

Labels remain pure, inspectable and 'fmap'-able; instances remain lawful.

Note: upon interpretation,

- effectful predicates will run /for each element/
- conditionals nested on the same element have short-circuiting behaviour
-}


{- | /Conditional inclusion/.

Transform each annotation so that, when interpreted as a wrapper around the element’s action, the element’s effects are only run if the effectful predicate evaluates to True.
-}
when' :: Monad m =>
     m Bool
  -> Silenceable m b -- ^ to include if True (else nothing)
  -> Silenceable m b
when' bM = mapAnn (Endo f <>)
  where
    f action = do
      b <- bM
      when b action

-- | /Conditional inclusion/ based on a pure predicate.
whenA :: Applicative f =>
     Bool
  -> Silenceable f b -- ^ to include if True (else nothing)
  -> Silenceable f b
whenA b = mapAnn (Endo (when b) <>)


{- | Flipped 'when''.

Example:

>>> import Data.Monoid (Endo(..))
>>> import Data.Char (toUpper)
>>> upper  = fmap toUpper
>>> interp = interpret (\ann -> appEndo ann . putStr) (>>) (pure ())
>>> yes    = string "yes" `onlyIf` (pure True )
>>> no     = string "no"  `onlyIf` (pure False)
>>> ok     = upper <$> (yes <> no <> string ".")
>>> interp ok
YES.
-}
onlyIf :: Monad m =>
     Silenceable m b
  -> m Bool
  -> Silenceable m b
onlyIf = flip when'
infixl 7 `onlyIf`


{- | /Binary choice/.

At interpretation, the monadic condition will be run twice (for every element).

The expectation that exactly one of the arguments will have all its elements included and the other have all its elements suppressed will hold if the same Bool is returned every time the effectful condition is run.
-}

ifThenElse :: Monad m =>
     m Bool
  -> Silenceable m b  -- ^ to include if True
  -> Silenceable m b  -- ^ to include if False
  -> Silenceable m b
ifThenElse pM true false =
     (when'        pM  true )
  <> (when' (not<$>pM) false)


-- ** Interpret

{- | Interpret by emitting each label with the given function, then applying the annotation, and combining with '>>'.

Note: this function is basically 'interpret', but with some type specialization, some defaults and an adjusted API shape.
-}
run :: Monad m =>
     (b -> m ())      -- ^ emitting a label
  -> Silenceable m b  -- ^ the t'Silenceable' to interpret
  -> m ()             -- ^ returned interpretation
run emit = interpret f (>>) z
  where
    f ann b = appEndo ann (emit b)
    z       = pure ()
