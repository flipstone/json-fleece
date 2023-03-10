{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.CompanyFull.MattePaintingCompany
  ( MattePaintingCompany(..)
  , mattePaintingCompanySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype MattePaintingCompany = MattePaintingCompany Bool
  deriving (Show, Eq)

mattePaintingCompanySchema :: FC.Fleece schema => schema MattePaintingCompany
mattePaintingCompanySchema =
  FC.coerceSchema FC.boolean