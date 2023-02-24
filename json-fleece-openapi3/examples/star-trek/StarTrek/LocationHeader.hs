{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.LocationHeader
  ( LocationHeader(..)
  , locationHeaderSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Show)

data LocationHeader = LocationHeader
  { name :: Text -- ^ Location name
  , uid :: Text -- ^ Location unique ID
  }
  deriving (Eq, Show)

locationHeaderSchema :: FC.Fleece schema => schema LocationHeader
locationHeaderSchema =
  FC.object $
    FC.constructor LocationHeader
      #+ FC.required "name" name FC.text
      #+ FC.required "uid" uid FC.text