{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.FoodBase
  ( FoodBase(..)
  , foodBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.FoodBase.AlcoholicBeverage (AlcoholicBeverage, alcoholicBeverageSchema)
import StarTrek.FoodBase.Beverage (Beverage, beverageSchema)
import StarTrek.FoodBase.Dessert (Dessert, dessertSchema)
import StarTrek.FoodBase.EarthlyOrigin (EarthlyOrigin, earthlyOriginSchema)
import StarTrek.FoodBase.Fruit (Fruit, fruitSchema)
import StarTrek.FoodBase.HerbOrSpice (HerbOrSpice, herbOrSpiceSchema)
import StarTrek.FoodBase.Juice (Juice, juiceSchema)
import StarTrek.FoodBase.Name (Name, nameSchema)
import StarTrek.FoodBase.Sauce (Sauce, sauceSchema)
import StarTrek.FoodBase.Soup (Soup, soupSchema)
import StarTrek.FoodBase.Tea (Tea, teaSchema)
import StarTrek.FoodBase.Uid (Uid, uidSchema)

data FoodBase = FoodBase
  { name :: Name -- ^ Food name
  , tea :: Maybe Tea -- ^ Whether it's a tea
  , juice :: Maybe Juice -- ^ Whether it's a juice
  , uid :: Uid -- ^ Food unique ID
  , fruit :: Maybe Fruit -- ^ Whether it's a fruit
  , dessert :: Maybe Dessert -- ^ Whether it's a dessert
  , herbOrSpice :: Maybe HerbOrSpice -- ^ Whether it's a herb or a spice
  , beverage :: Maybe Beverage -- ^ Whether it's a beverage
  , soup :: Maybe Soup -- ^ Whether it's a soup
  , earthlyOrigin :: Maybe EarthlyOrigin -- ^ Whether it's of earthly origin
  , sauce :: Maybe Sauce -- ^ Whether it's a sauce
  , alcoholicBeverage :: Maybe AlcoholicBeverage -- ^ Whether it's an alcoholic beverage
  }
  deriving (Eq, Show)

foodBaseSchema :: FC.Fleece schema => schema FoodBase
foodBaseSchema =
  FC.object $
    FC.constructor FoodBase
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