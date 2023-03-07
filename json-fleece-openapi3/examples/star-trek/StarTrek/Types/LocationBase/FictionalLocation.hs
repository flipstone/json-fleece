{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.LocationBase.FictionalLocation
  ( FictionalLocation(..)
  , fictionalLocationSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype FictionalLocation = FictionalLocation Bool
  deriving (Show, Eq)

fictionalLocationSchema :: FC.Fleece schema => schema FictionalLocation
fictionalLocationSchema =
  FC.coerceSchema FC.boolean