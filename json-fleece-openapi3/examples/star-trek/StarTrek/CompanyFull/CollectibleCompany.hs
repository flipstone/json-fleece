{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CompanyFull.CollectibleCompany
  ( CollectibleCompany(..)
  , collectibleCompanySchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype CollectibleCompany = CollectibleCompany Bool
  deriving (Show, Eq)

collectibleCompanySchema :: FC.Fleece schema => schema CollectibleCompany
collectibleCompanySchema =
  FC.coerceSchema FC.boolean