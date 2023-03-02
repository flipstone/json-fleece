{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.OccupationFull.LegalOccupation
  ( LegalOccupation(..)
  , legalOccupationSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype LegalOccupation = LegalOccupation Bool
  deriving (Show, Eq)

legalOccupationSchema :: FC.Fleece schema => schema LegalOccupation
legalOccupationSchema =
  FC.coerceSchema FC.boolean