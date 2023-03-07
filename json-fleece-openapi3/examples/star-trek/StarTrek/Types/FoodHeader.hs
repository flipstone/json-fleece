{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.FoodHeader
  ( FoodHeader(..)
  , foodHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.Types.FoodHeader.Name as Name
import qualified StarTrek.Types.FoodHeader.Uid as Uid

data FoodHeader = FoodHeader
  { name :: Name.Name -- ^ Food name
  , uid :: Uid.Uid -- ^ Food unique ID
  }
  deriving (Eq, Show)

foodHeaderSchema :: FC.Fleece schema => schema FoodHeader
foodHeaderSchema =
  FC.object $
    FC.constructor FoodHeader
      #+ FC.required "name" name Name.nameSchema
      #+ FC.required "uid" uid Uid.uidSchema