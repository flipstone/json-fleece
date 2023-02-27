{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TechnologyHeader
  ( TechnologyHeader(..)
  , technologyHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import StarTrek.TechnologyHeader.Name (Name, nameSchema)
import StarTrek.TechnologyHeader.Uid (Uid, uidSchema)

data TechnologyHeader = TechnologyHeader
  { name :: Name -- ^ Technology name
  , uid :: Uid -- ^ Technology unique ID
  }
  deriving (Eq, Show)

technologyHeaderSchema :: FC.Fleece schema => schema TechnologyHeader
technologyHeaderSchema =
  FC.object $
    FC.constructor TechnologyHeader
      #+ FC.required "name" name nameSchema
      #+ FC.required "uid" uid uidSchema