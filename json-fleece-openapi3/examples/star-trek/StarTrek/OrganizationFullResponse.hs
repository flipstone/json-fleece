{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.OrganizationFullResponse
  ( OrganizationFullResponse(..)
  , organizationFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.OrganizationFull (OrganizationFull, organizationFullSchema)

data OrganizationFullResponse = OrganizationFullResponse
  { organization :: Maybe OrganizationFull -- ^ Full organization, returned when queried using UID
  }
  deriving (Eq, Show)

organizationFullResponseSchema :: FC.Fleece schema => schema OrganizationFullResponse
organizationFullResponseSchema =
  FC.object $
    FC.constructor OrganizationFullResponse
      #+ FC.optional "organization" organization organizationFullSchema