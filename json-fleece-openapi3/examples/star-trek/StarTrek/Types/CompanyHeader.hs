{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.CompanyHeader
  ( CompanyHeader(..)
  , companyHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.Types.CompanyHeader.Name as Name
import qualified StarTrek.Types.CompanyHeader.Uid as Uid

data CompanyHeader = CompanyHeader
  { uid :: Uid.Uid -- ^ Company unique ID
  , name :: Name.Name -- ^ Company title
  }
  deriving (Eq, Show)

companyHeaderSchema :: FC.Fleece schema => schema CompanyHeader
companyHeaderSchema =
  FC.object $
    FC.constructor CompanyHeader
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.required "name" name Name.nameSchema