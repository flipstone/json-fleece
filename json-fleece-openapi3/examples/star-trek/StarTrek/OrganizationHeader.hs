{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.OrganizationHeader
  ( OrganizationHeader(..)
  , organizationHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import StarTrek.OrganizationHeader.Name (Name, nameSchema)
import StarTrek.OrganizationHeader.Uid (Uid, uidSchema)

data OrganizationHeader = OrganizationHeader
  { name :: Name -- ^ Organization name
  , uid :: Uid -- ^ Organization unique ID
  }
  deriving (Eq, Show)

organizationHeaderSchema :: FC.Fleece schema => schema OrganizationHeader
organizationHeaderSchema =
  FC.object $
    FC.constructor OrganizationHeader
      #+ FC.required "name" name nameSchema
      #+ FC.required "uid" uid uidSchema