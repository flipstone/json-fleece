{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.OrganizationHeader
  ( OrganizationHeader(..)
  , organizationHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.Types.OrganizationHeader.Name as Name
import qualified StarTrek.Types.OrganizationHeader.Uid as Uid

data OrganizationHeader = OrganizationHeader
  { name :: Name.Name -- ^ Organization name
  , uid :: Uid.Uid -- ^ Organization unique ID
  }
  deriving (Eq, Show)

organizationHeaderSchema :: FC.Fleece schema => schema OrganizationHeader
organizationHeaderSchema =
  FC.object $
    FC.constructor OrganizationHeader
      #+ FC.required "name" name Name.nameSchema
      #+ FC.required "uid" uid Uid.uidSchema