{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.OrganizationBaseResponse
  ( OrganizationBaseResponse(..)
  , organizationBaseResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.OrganizationBase (OrganizationBase, organizationBaseSchema)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)

data OrganizationBaseResponse = OrganizationBaseResponse
  { sort :: Maybe ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage -- ^ Object describing response page
  , organizations :: Maybe [OrganizationBase] -- ^ List of organizations matching given criteria
  }
  deriving (Eq, Show)

organizationBaseResponseSchema :: FC.Fleece schema => schema OrganizationBaseResponse
organizationBaseResponseSchema =
  FC.object $
    FC.constructor OrganizationBaseResponse
      #+ FC.optionalField FC.OmitKey_DelegateNull "sort" sort responseSortSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "page" page responsePageSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "organizations" organizations (FC.list organizationBaseSchema)