{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.PerformerFull.Ds9Performer
  ( Ds9Performer(..)
  , ds9PerformerSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Ds9Performer = Ds9Performer Bool
  deriving (Show, Eq)

ds9PerformerSchema :: FC.Fleece t => FC.Schema t Ds9Performer
ds9PerformerSchema =
  FC.coerceSchema FC.boolean