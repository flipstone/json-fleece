{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpacecraftClassHeader
  ( SpacecraftClassHeader(..)
  , spacecraftClassHeaderSchema
  ) where

import Data.Text (Text)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)

data SpacecraftClassHeader = SpacecraftClassHeader
  { name :: Text -- ^ Spacecraft class name
  , uid :: Text -- ^ Spacecraft class unique ID
  }
  deriving (Eq, Show)

spacecraftClassHeaderSchema :: FC.Fleece schema => schema SpacecraftClassHeader
spacecraftClassHeaderSchema =
  FC.object $
    FC.constructor SpacecraftClassHeader
      #+ FC.required "name" name FC.text
      #+ FC.required "uid" uid FC.text