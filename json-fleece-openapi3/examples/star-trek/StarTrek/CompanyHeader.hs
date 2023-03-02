{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CompanyHeader
  ( CompanyHeader(..)
  , companyHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.CompanyHeader.Name as Name
import qualified StarTrek.CompanyHeader.Uid as Uid

data CompanyHeader = CompanyHeader
  { name :: Name.Name -- ^ Company title
  , uid :: Uid.Uid -- ^ Company unique ID
  }
  deriving (Eq, Show)

companyHeaderSchema :: FC.Fleece schema => schema CompanyHeader
companyHeaderSchema =
  FC.object $
    FC.constructor CompanyHeader
      #+ FC.required "name" name Name.nameSchema
      #+ FC.required "uid" uid Uid.uidSchema