{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Country
  ( Country(..)
  , countrySchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Country.Iso31661Alpha2Code as Iso31661Alpha2Code
import qualified StarTrek.Country.Name as Name
import qualified StarTrek.Country.Uid as Uid

data Country = Country
  { name :: Maybe Name.Name -- ^ Country name
  , uid :: Maybe Uid.Uid -- ^ Country unique ID
  , iso31661Alpha2Code :: Maybe Iso31661Alpha2Code.Iso31661Alpha2Code -- ^ ISO 3166-1 alpha-2 code
  }
  deriving (Eq, Show)

countrySchema :: FC.Fleece schema => schema Country
countrySchema =
  FC.object $
    FC.constructor Country
      #+ FC.optional "name" name Name.nameSchema
      #+ FC.optional "uid" uid Uid.uidSchema
      #+ FC.optional "iso31661Alpha2Code" iso31661Alpha2Code Iso31661Alpha2Code.iso31661Alpha2CodeSchema