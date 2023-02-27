{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CharacterHeader
  ( CharacterHeader(..)
  , characterHeaderSchema
  ) where

import Data.Text (Text)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)

data CharacterHeader = CharacterHeader
  { name :: Text -- ^ Character name
  , uid :: Text -- ^ Character unique ID
  }
  deriving (Eq, Show)

characterHeaderSchema :: FC.Fleece schema => schema CharacterHeader
characterHeaderSchema =
  FC.object $
    FC.constructor CharacterHeader
      #+ FC.required "name" name FC.text
      #+ FC.required "uid" uid FC.text