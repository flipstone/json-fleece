{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.OrganizationHeader
  ( OrganizationHeader(..)
  , organizationHeaderSchema
  ) where

import Data.Text (Text)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)

data OrganizationHeader = OrganizationHeader
  { name :: Text -- ^ Organization name
  , uid :: Text -- ^ Organization unique ID
  }
  deriving (Eq, Show)

organizationHeaderSchema :: FC.Fleece schema => schema OrganizationHeader
organizationHeaderSchema =
  FC.object $
    FC.constructor OrganizationHeader
      #+ FC.required "name" name FC.text
      #+ FC.required "uid" uid FC.text