{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CompanyBaseResponse
  ( CompanyBaseResponse(..)
  , companyBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.CompanyBase (CompanyBase, companyBaseSchema)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)

data CompanyBaseResponse = CompanyBaseResponse
  { sort :: Maybe ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage -- ^ Object describing response page
  , companies :: Maybe [CompanyBase] -- ^ Base company, returned in search results
  }
  deriving (Eq, Show)

companyBaseResponseSchema :: FC.Fleece schema => schema CompanyBaseResponse
companyBaseResponseSchema =
  FC.object $
    FC.constructor CompanyBaseResponse
      #+ FC.optional "sort" sort responseSortSchema
      #+ FC.optional "page" page responsePageSchema
      #+ FC.optional "companies" companies (FC.list companyBaseSchema)