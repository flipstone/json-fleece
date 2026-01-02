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
  { name :: Name.Name -- ^ Company title
  , uid :: Uid.Uid -- ^ Company unique ID
  }
  deriving (Eq, Show)

companyHeaderSchema :: FC.Fleece t => FC.Schema t CompanyHeader
companyHeaderSchema =
  FC.object $
    FC.constructor CompanyHeader
      #+ FC.required "name" name Name.nameSchema
      #+ FC.required "uid" uid Uid.uidSchema