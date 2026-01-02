{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.CompanyBase.FilmEquipmentCompany
  ( FilmEquipmentCompany(..)
  , filmEquipmentCompanySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype FilmEquipmentCompany = FilmEquipmentCompany Bool
  deriving (Show, Eq)

filmEquipmentCompanySchema :: FC.Fleece t => FC.Schema t FilmEquipmentCompany
filmEquipmentCompanySchema =
  FC.coerceSchema FC.boolean