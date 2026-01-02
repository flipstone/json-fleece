{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.OrganizationBaseResponse
  ( OrganizationBaseResponse(..)
  , organizationBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.OrganizationBase as OrganizationBase
import qualified StarTrek.Types.ResponsePage as ResponsePage
import qualified StarTrek.Types.ResponseSort as ResponseSort

data OrganizationBaseResponse = OrganizationBaseResponse
  { organizations :: Maybe [OrganizationBase.OrganizationBase] -- ^ Base organization, returned in search results
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  }
  deriving (Eq, Show)

organizationBaseResponseSchema :: FC.Fleece t => FC.Schema t OrganizationBaseResponse
organizationBaseResponseSchema =
  FC.object $
    FC.constructor OrganizationBaseResponse
      #+ FC.optional "organizations" organizations (FC.list OrganizationBase.organizationBaseSchema)
      #+ FC.optional "page" page ResponsePage.responsePageSchema
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema