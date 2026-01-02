{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.CompanyBase.SpecialEffectsCompany
  ( SpecialEffectsCompany(..)
  , specialEffectsCompanySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype SpecialEffectsCompany = SpecialEffectsCompany Bool
  deriving (Show, Eq)

specialEffectsCompanySchema :: FC.Fleece t => FC.Schema t SpecialEffectsCompany
specialEffectsCompanySchema =
  FC.coerceSchema FC.boolean