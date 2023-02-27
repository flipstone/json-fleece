{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.FoodHeader
  ( FoodHeader(..)
  , foodHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import StarTrek.FoodHeader.Name (Name, nameSchema)
import StarTrek.FoodHeader.Uid (Uid, uidSchema)

data FoodHeader = FoodHeader
  { name :: Name -- ^ Food name
  , uid :: Uid -- ^ Food unique ID
  }
  deriving (Eq, Show)

foodHeaderSchema :: FC.Fleece schema => schema FoodHeader
foodHeaderSchema =
  FC.object $
    FC.constructor FoodHeader
      #+ FC.required "name" name nameSchema
      #+ FC.required "uid" uid uidSchema