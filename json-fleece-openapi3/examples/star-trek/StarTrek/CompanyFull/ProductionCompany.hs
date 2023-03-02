{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CompanyFull.ProductionCompany
  ( ProductionCompany(..)
  , productionCompanySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ProductionCompany = ProductionCompany Bool
  deriving (Show, Eq)

productionCompanySchema :: FC.Fleece schema => schema ProductionCompany
productionCompanySchema =
  FC.coerceSchema FC.boolean