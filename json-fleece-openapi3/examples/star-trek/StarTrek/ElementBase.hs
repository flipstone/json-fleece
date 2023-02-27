{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ElementBase
  ( ElementBase(..)
  , elementBaseSchema
  ) where

import Data.Text (Text)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Bool, Eq, Integer, Maybe, Show)

data ElementBase = ElementBase
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

elementBaseSchema :: FC.Fleece schema => schema ElementBase
elementBaseSchema =
  FC.object $
    FC.constructor ElementBase
      #+ FC.optional "hypersonicSeries" hypersonicSeries FC.boolean
      #+ FC.required "name" name FC.text
      #+ FC.optional "atomicNumber" atomicNumber FC.integer
      #+ FC.optional "megaSeries" megaSeries FC.boolean
      #+ FC.optional "symbol" symbol FC.text
      #+ FC.optional "atomicWeight" atomicWeight FC.integer
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "transonicSeries" transonicSeries FC.boolean
      #+ FC.optional "transuranium" transuranium FC.boolean
      #+ FC.optional "gammaSeries" gammaSeries FC.boolean
      #+ FC.optional "omegaSeries" omegaSeries FC.boolean
      #+ FC.optional "worldSeries" worldSeries FC.boolean