{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.AstronomicalObjectHeader
  ( AstronomicalObjectHeader(..)
  , astronomicalObjectHeaderSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Show)

data AstronomicalObjectHeader = AstronomicalObjectHeader
  { name :: Text -- ^ Astronomical object name
  , uid :: Text -- ^ Astronomical object's unique ID
  }
  deriving (Eq, Show)

astronomicalObjectHeaderSchema :: FC.Fleece schema => schema AstronomicalObjectHeader
astronomicalObjectHeaderSchema =
  FC.object $
    FC.constructor AstronomicalObjectHeader
      #+ FC.required "name" name FC.text
      #+ FC.required "uid" uid FC.text