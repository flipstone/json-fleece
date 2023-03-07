{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.OccupationFull.MedicalOccupation
  ( MedicalOccupation(..)
  , medicalOccupationSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype MedicalOccupation = MedicalOccupation Bool
  deriving (Show, Eq)

medicalOccupationSchema :: FC.Fleece schema => schema MedicalOccupation
medicalOccupationSchema =
  FC.coerceSchema FC.boolean