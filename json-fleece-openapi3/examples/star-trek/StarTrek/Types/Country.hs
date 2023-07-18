{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.Country
  ( Country(..)
  , countrySchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.Country.Iso31661Alpha2Code as Iso31661Alpha2Code
import qualified StarTrek.Types.Country.Name as Name
import qualified StarTrek.Types.Country.Uid as Uid

data Country = Country
  { uid :: Maybe Uid.Uid -- ^ Country unique ID
  , iso31661Alpha2Code :: Maybe Iso31661Alpha2Code.Iso31661Alpha2Code -- ^ ISO 3166-1 alpha-2 code
  , name :: Maybe Name.Name -- ^ Country name
  }
  deriving (Eq, Show)

countrySchema :: FC.Fleece schema => schema Country
countrySchema =
  FC.object $
    FC.constructor Country
      #+ FC.optional "uid" uid Uid.uidSchema
      #+ FC.optional "iso31661Alpha2Code" iso31661Alpha2Code Iso31661Alpha2Code.iso31661Alpha2CodeSchema
      #+ FC.optional "name" name Name.nameSchema