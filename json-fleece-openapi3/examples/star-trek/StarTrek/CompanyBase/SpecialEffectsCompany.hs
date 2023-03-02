{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CompanyBase.SpecialEffectsCompany
  ( SpecialEffectsCompany(..)
  , specialEffectsCompanySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype SpecialEffectsCompany = SpecialEffectsCompany Bool
  deriving (Show, Eq)

specialEffectsCompanySchema :: FC.Fleece schema => schema SpecialEffectsCompany
specialEffectsCompanySchema =
  FC.coerceSchema FC.boolean