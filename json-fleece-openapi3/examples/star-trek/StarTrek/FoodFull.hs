{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.FoodFull
  ( FoodFull(..)
  , foodFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.FoodFull.AlcoholicBeverage (AlcoholicBeverage, alcoholicBeverageSchema)
import StarTrek.FoodFull.Beverage (Beverage, beverageSchema)
import StarTrek.FoodFull.Dessert (Dessert, dessertSchema)
import StarTrek.FoodFull.EarthlyOrigin (EarthlyOrigin, earthlyOriginSchema)
import StarTrek.FoodFull.Fruit (Fruit, fruitSchema)
import StarTrek.FoodFull.HerbOrSpice (HerbOrSpice, herbOrSpiceSchema)
import StarTrek.FoodFull.Juice (Juice, juiceSchema)
import StarTrek.FoodFull.Name (Name, nameSchema)
import StarTrek.FoodFull.Sauce (Sauce, sauceSchema)
import StarTrek.FoodFull.Soup (Soup, soupSchema)
import StarTrek.FoodFull.Tea (Tea, teaSchema)
import StarTrek.FoodFull.Uid (Uid, uidSchema)

data FoodFull = FoodFull
  { name :: Name -- ^ Food name
  , tea :: Maybe Tea -- ^ Whether it's a tea
  , juice :: Maybe Juice -- ^ Whether it's a juice
  , uid :: Uid -- ^ Food unique ID
  , fruit :: Maybe Fruit -- ^ Whether it's a fruit
  , dessert :: Maybe Dessert -- ^ Whether it's a dessert
  , herbOrSpice :: Maybe HerbOrSpice -- ^ Whether it's an herb or a spice
  , beverage :: Maybe Beverage -- ^ Whether it's a beverage
  , soup :: Maybe Soup -- ^ Whether it's a soup
  , earthlyOrigin :: Maybe EarthlyOrigin -- ^ Whether it's of earthly origin
  , sauce :: Maybe Sauce -- ^ Whether it's a sauce
  , alcoholicBeverage :: Maybe AlcoholicBeverage -- ^ Whether it's an alcoholic beverage
  }
  deriving (Eq, Show)

foodFullSchema :: FC.Fleece schema => schema FoodFull
foodFullSchema =
  FC.object $
    FC.constructor FoodFull
      #+ FC.required "name" name nameSchema
      #+ FC.optional "tea" tea teaSchema
      #+ FC.optional "juice" juice juiceSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "fruit" fruit fruitSchema
      #+ FC.optional "dessert" dessert dessertSchema
      #+ FC.optional "herbOrSpice" herbOrSpice herbOrSpiceSchema
      #+ FC.optional "beverage" beverage beverageSchema
      #+ FC.optional "soup" soup soupSchema
      #+ FC.optional "earthlyOrigin" earthlyOrigin earthlyOriginSchema
      #+ FC.optional "sauce" sauce sauceSchema
      #+ FC.optional "alcoholicBeverage" alcoholicBeverage alcoholicBeverageSchema