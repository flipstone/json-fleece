{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CompanyBase.PostProductionCompany
  ( PostProductionCompany(..)
  , postProductionCompanySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype PostProductionCompany = PostProductionCompany Bool
  deriving (Show, Eq)

postProductionCompanySchema :: FC.Fleece schema => schema PostProductionCompany
postProductionCompanySchema =
  FC.coerceSchema FC.boolean