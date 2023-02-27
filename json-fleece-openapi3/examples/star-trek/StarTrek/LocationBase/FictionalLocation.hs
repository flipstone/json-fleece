{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.LocationBase.FictionalLocation
  ( FictionalLocation(..)
  , fictionalLocationSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype FictionalLocation = FictionalLocation Bool
  deriving (Show, Eq)

fictionalLocationSchema :: FC.Fleece schema => schema FictionalLocation
fictionalLocationSchema =
  FC.coerceSchema FC.boolean