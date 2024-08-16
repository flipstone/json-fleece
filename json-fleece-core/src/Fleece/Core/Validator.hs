{-# LANGUAGE FlexibleContexts #-}

module Fleece.Core.Validator
  ( FleeceValidator (..)
  , coercion
  , transform
  , mapCheck
  , mapUncheck
  , identity
  , StandardValidator (..)
  , NoOpValidator (..)
  ) where

import Control.Monad ((<=<))
import qualified Data.Coerce as Coerce

class FleeceValidator validator where
  mkValidator :: (b -> a) -> (a -> Either String b) -> validator a b
  compose :: validator b c -> validator a b -> validator a c

coercion :: (Coerce.Coercible a b, FleeceValidator validator) => validator a b
coercion = mkValidator Coerce.coerce (pure . Coerce.coerce)

transform :: FleeceValidator validator => (b -> a) -> (a -> b) -> validator a b
transform f g = mkValidator f (pure . g)

mapCheck :: FleeceValidator validator => (c -> b) -> (b -> c) -> validator a b -> validator a c
mapCheck f g v = mkValidator f (pure . g) `compose` v

mapUncheck :: FleeceValidator validator => (c -> a) -> (a -> c) -> validator c b -> validator a b
mapUncheck f g v = v `compose` mkValidator f (pure . g)

identity :: FleeceValidator validator => validator a a
identity = mkValidator id pure

data StandardValidator a b = StandardValidator
  { uncheck :: b -> a
  , check :: a -> Either String b
  }

instance FleeceValidator StandardValidator where
  mkValidator = StandardValidator
  compose (StandardValidator f1 g1) (StandardValidator f2 g2) = StandardValidator (f2 . f1) (g1 <=< g2)

data NoOpValidator a b = NoOpValidator

instance FleeceValidator NoOpValidator where
  mkValidator _ _ = NoOpValidator
  compose _ _ = NoOpValidator
