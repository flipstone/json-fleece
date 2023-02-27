{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpacecraftType
  ( SpacecraftType(..)
  , spacecraftTypeSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)

data SpacecraftType = SpacecraftType
  { name :: Maybe Text -- ^ Spacecraft type name
  , uid :: Maybe Text -- ^ Spacecraft type unique ID
  }
  deriving (Eq, Show)

spacecraftTypeSchema :: FC.Fleece schema => schema SpacecraftType
spacecraftTypeSchema =
  FC.object $
    FC.constructor SpacecraftType
      #+ FC.optional "name" name FC.text
      #+ FC.optional "uid" uid FC.text