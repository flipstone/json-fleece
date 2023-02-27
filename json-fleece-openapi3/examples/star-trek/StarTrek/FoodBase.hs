{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.FoodBase
  ( FoodBase(..)
  , foodBaseSchema
  ) where

import Data.Text (Text)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Bool, Eq, Maybe, Show)

data FoodBase = FoodBase
  { name :: Text -- ^ Food name
  , tea :: Maybe Bool -- ^ Whether it's a tea
  , juice :: Maybe Bool -- ^ Whether it's a juice
  , uid :: Text -- ^ Food unique ID
  , fruit :: Maybe Bool -- ^ Whether it's a fruit
  , dessert :: Maybe Bool -- ^ Whether it's a dessert
  , herbOrSpice :: Maybe Bool -- ^ Whether it's a herb or a spice
  , beverage :: Maybe Bool -- ^ Whether it's a beverage
  , soup :: Maybe Bool -- ^ Whether it's a soup
  , earthlyOrigin :: Maybe Bool -- ^ Whether it's of earthly origin
  , sauce :: Maybe Bool -- ^ Whether it's a sauce
  , alcoholicBeverage :: Maybe Bool -- ^ Whether it's an alcoholic beverage
  }
  deriving (Eq, Show)

foodBaseSchema :: FC.Fleece schema => schema FoodBase
foodBaseSchema =
  FC.object $
    FC.constructor FoodBase
      #+ FC.required "name" name FC.text
      #+ FC.optional "tea" tea FC.boolean
      #+ FC.optional "juice" juice FC.boolean
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "fruit" fruit FC.boolean
      #+ FC.optional "dessert" dessert FC.boolean
      #+ FC.optional "herbOrSpice" herbOrSpice FC.boolean
      #+ FC.optional "beverage" beverage FC.boolean
      #+ FC.optional "soup" soup FC.boolean
      #+ FC.optional "earthlyOrigin" earthlyOrigin FC.boolean
      #+ FC.optional "sauce" sauce FC.boolean
      #+ FC.optional "alcoholicBeverage" alcoholicBeverage FC.boolean