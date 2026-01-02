{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TechnologyHeader
  ( TechnologyHeader(..)
  , technologyHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.Types.TechnologyHeader.Name as Name
import qualified StarTrek.Types.TechnologyHeader.Uid as Uid

data TechnologyHeader = TechnologyHeader
  { name :: Name.Name -- ^ Technology name
  , uid :: Uid.Uid -- ^ Technology unique ID
  }
  deriving (Eq, Show)

technologyHeaderSchema :: FC.Fleece t => FC.Schema t TechnologyHeader
technologyHeaderSchema =
  FC.object $
    FC.constructor TechnologyHeader
      #+ FC.required "name" name Name.nameSchema
      #+ FC.required "uid" uid Uid.uidSchema