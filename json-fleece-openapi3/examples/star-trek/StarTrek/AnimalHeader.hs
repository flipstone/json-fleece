{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.AnimalHeader
  ( AnimalHeader(..)
  , animalHeaderSchema
  ) where

import Data.Text (Text)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)

data AnimalHeader = AnimalHeader
  { name :: Text -- ^ Animal name
  , uid :: Text -- ^ Animal unique ID
  }
  deriving (Eq, Show)

animalHeaderSchema :: FC.Fleece schema => schema AnimalHeader
animalHeaderSchema =
  FC.object $
    FC.constructor AnimalHeader
      #+ FC.required "name" name FC.text
      #+ FC.required "uid" uid FC.text