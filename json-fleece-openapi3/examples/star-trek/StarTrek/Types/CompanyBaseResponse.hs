{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.CompanyBaseResponse
  ( CompanyBaseResponse(..)
  , companyBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.CompanyBase as CompanyBase
import qualified StarTrek.Types.ResponsePage as ResponsePage
import qualified StarTrek.Types.ResponseSort as ResponseSort

data CompanyBaseResponse = CompanyBaseResponse
  { page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  , companies :: Maybe [CompanyBase.CompanyBase] -- ^ Base company, returned in search results
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  }
  deriving (Eq, Show)

companyBaseResponseSchema :: FC.Fleece schema => schema CompanyBaseResponse
companyBaseResponseSchema =
  FC.object $
    FC.constructor CompanyBaseResponse
      #+ FC.optional "page" page ResponsePage.responsePageSchema
      #+ FC.optional "companies" companies (FC.list CompanyBase.companyBaseSchema)
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema