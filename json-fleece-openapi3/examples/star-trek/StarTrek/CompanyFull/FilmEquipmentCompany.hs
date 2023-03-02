{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CompanyFull.FilmEquipmentCompany
  ( FilmEquipmentCompany(..)
  , filmEquipmentCompanySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype FilmEquipmentCompany = FilmEquipmentCompany Bool
  deriving (Show, Eq)

filmEquipmentCompanySchema :: FC.Fleece schema => schema FilmEquipmentCompany
filmEquipmentCompanySchema =
  FC.coerceSchema FC.boolean