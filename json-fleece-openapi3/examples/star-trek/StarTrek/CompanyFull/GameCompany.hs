{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CompanyFull.GameCompany
  ( GameCompany(..)
  , gameCompanySchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype GameCompany = GameCompany Bool
  deriving (Show, Eq)

gameCompanySchema :: FC.Fleece schema => schema GameCompany
gameCompanySchema =
  FC.coerceSchema FC.boolean