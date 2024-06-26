{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.CharacterHeader
  ( CharacterHeader(..)
  , characterHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.Types.CharacterHeader.Name as Name
import qualified StarTrek.Types.CharacterHeader.Uid as Uid

data CharacterHeader = CharacterHeader
  { uid :: Uid.Uid -- ^ Character unique ID
  , name :: Name.Name -- ^ Character name
  }
  deriving (Eq, Show)

characterHeaderSchema :: FC.Fleece schema => schema CharacterHeader
characterHeaderSchema =
  FC.object $
    FC.constructor CharacterHeader
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.required "name" name Name.nameSchema