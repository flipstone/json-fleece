{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CompanyBaseResponse
  ( CompanyBaseResponse(..)
  , companyBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.CompanyBase as CompanyBase
import qualified StarTrek.ResponsePage as ResponsePage
import qualified StarTrek.ResponseSort as ResponseSort

data CompanyBaseResponse = CompanyBaseResponse
  { sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  , companies :: Maybe [CompanyBase.CompanyBase] -- ^ Base company, returned in search results
  }
  deriving (Eq, Show)

companyBaseResponseSchema :: FC.Fleece schema => schema CompanyBaseResponse
companyBaseResponseSchema =
  FC.object $
    FC.constructor CompanyBaseResponse
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema
      #+ FC.optional "page" page ResponsePage.responsePageSchema
      #+ FC.optional "companies" companies (FC.list CompanyBase.companyBaseSchema)