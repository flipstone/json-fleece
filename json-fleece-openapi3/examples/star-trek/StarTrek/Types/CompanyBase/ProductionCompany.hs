{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.CompanyBase.ProductionCompany
  ( ProductionCompany(..)
  , productionCompanySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ProductionCompany = ProductionCompany Bool
  deriving (Show, Eq)

productionCompanySchema :: FC.Fleece t => FC.Schema t ProductionCompany
productionCompanySchema =
  FC.coerceSchema FC.boolean