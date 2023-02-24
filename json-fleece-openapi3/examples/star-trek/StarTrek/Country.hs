{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Country
  ( Country(..)
  , countrySchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)

data Country = Country
  { name :: Maybe Text -- ^ Country name
  , uid :: Maybe Text -- ^ Country unique ID
  , iso31661Alpha2Code :: Maybe Text -- ^ ISO 3166-1 alpha-2 code
  }
  deriving (Eq, Show)

countrySchema :: FC.Fleece schema => schema Country
countrySchema =
  FC.object $
    FC.constructor Country
      #+ FC.optionalField FC.OmitKey_DelegateNull "name" name FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "uid" uid FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "iso31661Alpha2Code" iso31661Alpha2Code FC.text