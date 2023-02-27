{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CompanyHeader
  ( CompanyHeader(..)
  , companyHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import StarTrek.CompanyHeader.Name (Name, nameSchema)
import StarTrek.CompanyHeader.Uid (Uid, uidSchema)

data CompanyHeader = CompanyHeader
  { name :: Name -- ^ Company title
  , uid :: Uid -- ^ Company unique ID
  }
  deriving (Eq, Show)

companyHeaderSchema :: FC.Fleece schema => schema CompanyHeader
companyHeaderSchema =
  FC.object $
    FC.constructor CompanyHeader
      #+ FC.required "name" name nameSchema
      #+ FC.required "uid" uid uidSchema