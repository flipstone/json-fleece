{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpeciesHeader
  ( SpeciesHeader(..)
  , speciesHeaderSchema
  ) where

import Data.Text (Text)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)

data SpeciesHeader = SpeciesHeader
  { name :: Text -- ^ Species name
  , uid :: Text -- ^ Species unique ID
  }
  deriving (Eq, Show)

speciesHeaderSchema :: FC.Fleece schema => schema SpeciesHeader
speciesHeaderSchema =
  FC.object $
    FC.constructor SpeciesHeader
      #+ FC.required "name" name FC.text
      #+ FC.required "uid" uid FC.text