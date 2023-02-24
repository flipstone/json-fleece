{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpacecraftHeader
  ( SpacecraftHeader(..)
  , spacecraftHeaderSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Show)

data SpacecraftHeader = SpacecraftHeader
  { name :: Text -- ^ Spacecraft name
  , uid :: Text -- ^ Spacecraft unique ID
  }
  deriving (Eq, Show)

spacecraftHeaderSchema :: FC.Fleece schema => schema SpacecraftHeader
spacecraftHeaderSchema =
  FC.object $
    FC.constructor SpacecraftHeader
      #+ FC.required "name" name FC.text
      #+ FC.required "uid" uid FC.text