{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.LocationFull.FictionalLocation
  ( FictionalLocation(..)
  , fictionalLocationSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype FictionalLocation = FictionalLocation Bool
  deriving (Show, Eq)

fictionalLocationSchema :: FC.Fleece t => FC.Schema t FictionalLocation
fictionalLocationSchema =
  FC.coerceSchema FC.boolean