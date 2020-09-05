{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{- |
   Module     : Composite.XStep
   License    : MIT
   Stability  : experimental

Tuple functions for composite records, inspired by relude.
-}

module Composite.XStep (
  RSource(runSource)
, RSource'
, XStep
, XStep'
, runSourceI
, runXStep
, prependXStep
) where

import Composite.Record
import Data.Functor.Identity
import Data.Vinyl
import Data.Vinyl.TypeLevel
import Data.Vinyl.XRec

-- | An `RSource` is simply a `ReaderT` transformer. This is a uniquely named
-- type as composte and vinyl aren't compatible via the `IsoHKD` interface due
-- to the way that `(:->)` is defined.
newtype RSource r m a = RSource { runSource :: r -> m a }

-- | The special case where the environment is a `Record`.
type RSource' r = RSource (Record r)

instance Functor m => IsoHKD (RSource r m) (s :-> a) where
  type HKD (RSource r m) (s :-> a) = r -> m a
  unHKD f = RSource $ fmap Val . f
  toHKD (RSource f) = fmap getVal . f

-- | An `XStep` is a n `XRec` with an `RSource`. The type is eta-reduced, but in practice signatures will be written `XStep m a b` to represent a Kleisli arrow from a to m b.
type XStep m a = XRec (RSource a m)

-- | The special case where the environment is a `Record`.
type XStep' m a = XStep m (Record a)

-- | Run an `RSource` against a `Record` and pull out the underlying functor.
runSourceI :: Functor m => RSource r m a -> r -> m (Identity a)
runSourceI x = fmap Identity . runSource x

-- | Run an `XStep` to completion.
runXStep :: (IsoXRec (RSource a m) b, Applicative m) => XStep m a b -> a -> m (Record b)
runXStep x y = rtraverse (`runSourceI` y) (fromXRec x)

-- | The special case where the environnent is a XStep environment is `Record`, and the result should be prepended extensibly.
prependXStep :: (IsoXRec (RSource' a m) b, Applicative m) => XStep' m a b -> Record a -> m (Record (b ++ a))
prependXStep f x = (<+> x) <$> runXStep f x
