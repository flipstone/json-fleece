{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.OrganizationBaseResponse
  ( OrganizationBaseResponse(..)
  , organizationBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.OrganizationBase as OrganizationBase
import qualified StarTrek.ResponsePage as ResponsePage
import qualified StarTrek.ResponseSort as ResponseSort

data OrganizationBaseResponse = OrganizationBaseResponse
  { sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  , organizations :: Maybe [OrganizationBase.OrganizationBase] -- ^ Base organization, returned in search results
  }
  deriving (Eq, Show)

organizationBaseResponseSchema :: FC.Fleece schema => schema OrganizationBaseResponse
organizationBaseResponseSchema =
  FC.object $
    FC.constructor OrganizationBaseResponse
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema
      #+ FC.optional "page" page ResponsePage.responsePageSchema
      #+ FC.optional "organizations" organizations (FC.list OrganizationBase.organizationBaseSchema)