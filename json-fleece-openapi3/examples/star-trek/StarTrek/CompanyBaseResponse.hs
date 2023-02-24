{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CompanyBaseResponse
  ( CompanyBaseResponse(..)
  , companyBaseResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.CompanyBase (CompanyBase, companyBaseSchema)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)

data CompanyBaseResponse = CompanyBaseResponse
  { sort :: Maybe ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage -- ^ Object describing response page
  , companies :: Maybe [CompanyBase] -- ^ List of companies matching given criteria
  }
  deriving (Eq, Show)

companyBaseResponseSchema :: FC.Fleece schema => schema CompanyBaseResponse
companyBaseResponseSchema =
  FC.object $
    FC.constructor CompanyBaseResponse
      #+ FC.optionalField FC.OmitKey_DelegateNull "sort" sort responseSortSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "page" page responsePageSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "companies" companies (FC.list companyBaseSchema)