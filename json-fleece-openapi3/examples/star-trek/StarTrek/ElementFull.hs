{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ElementFull
  ( ElementFull(..)
  , elementFullSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Bool, Eq, Integer, Maybe, Show)

data ElementFull = ElementFull
  { hypersonicSeries :: Maybe Bool -- ^ Whether it belongs to Hypersonic series
  , name :: Text -- ^ Element name
  , atomicNumber :: Maybe Integer -- ^ Element atomic number
  , megaSeries :: Maybe Bool -- ^ Whether it belongs to Mega series
  , symbol :: Maybe Text -- ^ Element symbol
  , atomicWeight :: Maybe Integer -- ^ Element atomic weight
  , uid :: Text -- ^ Element unique ID
  , transonicSeries :: Maybe Bool -- ^ Whether it belongs to Transonic series
  , transuranium :: Maybe Bool -- ^ Whether it's a transuranium
  , gammaSeries :: Maybe Bool -- ^ Whether it belongs to Gamma series
  , omegaSeries :: Maybe Bool -- ^ Whether it belongs to Omega series
  , worldSeries :: Maybe Bool -- ^ Whether it belongs to World series
  }
  deriving (Eq, Show)

elementFullSchema :: FC.Fleece schema => schema ElementFull
elementFullSchema =
  FC.object $
    FC.constructor ElementFull
      #+ FC.optionalField FC.OmitKey_DelegateNull "hypersonicSeries" hypersonicSeries FC.boolean
      #+ FC.required "name" name FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "atomicNumber" atomicNumber FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "megaSeries" megaSeries FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "symbol" symbol FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "atomicWeight" atomicWeight FC.integer
      #+ FC.required "uid" uid FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "transonicSeries" transonicSeries FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "transuranium" transuranium FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "gammaSeries" gammaSeries FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "omegaSeries" omegaSeries FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "worldSeries" worldSeries FC.boolean