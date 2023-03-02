{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CompanyBase.PropCompany
  ( PropCompany(..)
  , propCompanySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype PropCompany = PropCompany Bool
  deriving (Show, Eq)

propCompanySchema :: FC.Fleece schema => schema PropCompany
propCompanySchema =
  FC.coerceSchema FC.boolean