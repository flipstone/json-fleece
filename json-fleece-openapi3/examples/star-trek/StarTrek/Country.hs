{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Country
  ( Country(..)
  , countrySchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.Country.Iso31661Alpha2Code (Iso31661Alpha2Code, iso31661Alpha2CodeSchema)
import StarTrek.Country.Name (Name, nameSchema)
import StarTrek.Country.Uid (Uid, uidSchema)

data Country = Country
  { name :: Maybe Name -- ^ Country name
  , uid :: Maybe Uid -- ^ Country unique ID
  , iso31661Alpha2Code :: Maybe Iso31661Alpha2Code -- ^ ISO 3166-1 alpha-2 code
  }
  deriving (Eq, Show)

countrySchema :: FC.Fleece schema => schema Country
countrySchema =
  FC.object $
    FC.constructor Country
      #+ FC.optional "name" name nameSchema
      #+ FC.optional "uid" uid uidSchema
      #+ FC.optional "iso31661Alpha2Code" iso31661Alpha2Code iso31661Alpha2CodeSchema