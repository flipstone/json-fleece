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
  { name :: Name.Name -- ^ Character name
  , uid :: Uid.Uid -- ^ Character unique ID
  }
  deriving (Eq, Show)

characterHeaderSchema :: FC.Fleece schema => schema CharacterHeader
characterHeaderSchema =
  FC.object $
    FC.constructor CharacterHeader
      #+ FC.required "name" name Name.nameSchema
      #+ FC.required "uid" uid Uid.uidSchema