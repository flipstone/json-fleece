{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CharacterHeader
  ( CharacterHeader(..)
  , characterHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import StarTrek.CharacterHeader.Name (Name, nameSchema)
import StarTrek.CharacterHeader.Uid (Uid, uidSchema)

data CharacterHeader = CharacterHeader
  { name :: Name -- ^ Character name
  , uid :: Uid -- ^ Character unique ID
  }
  deriving (Eq, Show)

characterHeaderSchema :: FC.Fleece schema => schema CharacterHeader
characterHeaderSchema =
  FC.object $
    FC.constructor CharacterHeader
      #+ FC.required "name" name nameSchema
      #+ FC.required "uid" uid uidSchema