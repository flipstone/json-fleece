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
  { uid :: Uid.Uid -- ^ Technology unique ID
  , name :: Name.Name -- ^ Technology name
  }
  deriving (Eq, Show)

technologyHeaderSchema :: FC.Fleece schema => schema TechnologyHeader
technologyHeaderSchema =
  FC.object $
    FC.constructor TechnologyHeader
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.required "name" name Name.nameSchema