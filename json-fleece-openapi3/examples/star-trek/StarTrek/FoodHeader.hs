{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.FoodHeader
  ( FoodHeader(..)
  , foodHeaderSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Show)

data FoodHeader = FoodHeader
  { name :: Text -- ^ Food name
  , uid :: Text -- ^ Food unique ID
  }
  deriving (Eq, Show)

foodHeaderSchema :: FC.Fleece schema => schema FoodHeader
foodHeaderSchema =
  FC.object $
    FC.constructor FoodHeader
      #+ FC.required "name" name FC.text
      #+ FC.required "uid" uid FC.text