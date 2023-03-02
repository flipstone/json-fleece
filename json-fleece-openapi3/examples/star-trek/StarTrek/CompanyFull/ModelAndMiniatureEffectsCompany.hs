{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CompanyFull.ModelAndMiniatureEffectsCompany
  ( ModelAndMiniatureEffectsCompany(..)
  , modelAndMiniatureEffectsCompanySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ModelAndMiniatureEffectsCompany = ModelAndMiniatureEffectsCompany Bool
  deriving (Show, Eq)

modelAndMiniatureEffectsCompanySchema :: FC.Fleece schema => schema ModelAndMiniatureEffectsCompany
modelAndMiniatureEffectsCompanySchema =
  FC.coerceSchema FC.boolean