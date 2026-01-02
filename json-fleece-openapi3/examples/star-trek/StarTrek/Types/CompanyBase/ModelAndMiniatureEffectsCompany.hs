{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.CompanyBase.ModelAndMiniatureEffectsCompany
  ( ModelAndMiniatureEffectsCompany(..)
  , modelAndMiniatureEffectsCompanySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ModelAndMiniatureEffectsCompany = ModelAndMiniatureEffectsCompany Bool
  deriving (Show, Eq)

modelAndMiniatureEffectsCompanySchema :: FC.Fleece t => FC.Schema t ModelAndMiniatureEffectsCompany
modelAndMiniatureEffectsCompanySchema =
  FC.coerceSchema FC.boolean