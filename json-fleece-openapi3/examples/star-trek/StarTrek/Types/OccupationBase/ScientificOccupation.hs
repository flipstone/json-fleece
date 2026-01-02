{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.OccupationBase.ScientificOccupation
  ( ScientificOccupation(..)
  , scientificOccupationSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ScientificOccupation = ScientificOccupation Bool
  deriving (Show, Eq)

scientificOccupationSchema :: FC.Fleece t => FC.Schema t ScientificOccupation
scientificOccupationSchema =
  FC.coerceSchema FC.boolean