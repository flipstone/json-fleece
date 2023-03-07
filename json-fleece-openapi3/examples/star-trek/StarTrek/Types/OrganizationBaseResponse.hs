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