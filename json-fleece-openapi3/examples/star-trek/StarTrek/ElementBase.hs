{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ElementBase
  ( ElementBase(..)
  , elementBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ElementBase.AtomicNumber (AtomicNumber, atomicNumberSchema)
import StarTrek.ElementBase.AtomicWeight (AtomicWeight, atomicWeightSchema)
import StarTrek.ElementBase.GammaSeries (GammaSeries, gammaSeriesSchema)
import StarTrek.ElementBase.HypersonicSeries (HypersonicSeries, hypersonicSeriesSchema)
import StarTrek.ElementBase.MegaSeries (MegaSeries, megaSeriesSchema)
import StarTrek.ElementBase.Name (Name, nameSchema)
import StarTrek.ElementBase.OmegaSeries (OmegaSeries, omegaSeriesSchema)
import StarTrek.ElementBase.Symbol (Symbol, symbolSchema)
import StarTrek.ElementBase.TransonicSeries (TransonicSeries, transonicSeriesSchema)
import StarTrek.ElementBase.Transuranium (Transuranium, transuraniumSchema)
import StarTrek.ElementBase.Uid (Uid, uidSchema)
import StarTrek.ElementBase.WorldSeries (WorldSeries, worldSeriesSchema)

data ElementBase = ElementBase
  { hypersonicSeries :: Maybe HypersonicSeries -- ^ Whether it belongs to Hypersonic series
  , name :: Name -- ^ Element name
  , atomicNumber :: Maybe AtomicNumber -- ^ Element atomic number
  , megaSeries :: Maybe MegaSeries -- ^ Whether it belongs to Mega series
  , symbol :: Maybe Symbol -- ^ Element symbol
  , atomicWeight :: Maybe AtomicWeight -- ^ Element atomic weight
  , uid :: Uid -- ^ Element unique ID
  , transonicSeries :: Maybe TransonicSeries -- ^ Whether it belongs to Transonic series
  , transuranium :: Maybe Transuranium -- ^ Whether it's a transuranium
  , gammaSeries :: Maybe GammaSeries -- ^ Whether it belongs to Gamma series
  , omegaSeries :: Maybe OmegaSeries -- ^ Whether it belongs to Omega series
  , worldSeries :: Maybe WorldSeries -- ^ Whether it belongs to World series
  }
  deriving (Eq, Show)

elementBaseSchema :: FC.Fleece schema => schema ElementBase
elementBaseSchema =
  FC.object $
    FC.constructor ElementBase
      #+ FC.optional "hypersonicSeries" hypersonicSeries hypersonicSeriesSchema
      #+ FC.required "name" name nameSchema
      #+ FC.optional "atomicNumber" atomicNumber atomicNumberSchema
      #+ FC.optional "megaSeries" megaSeries megaSeriesSchema
      #+ FC.optional "symbol" symbol symbolSchema
      #+ FC.optional "atomicWeight" atomicWeight atomicWeightSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "transonicSeries" transonicSeries transonicSeriesSchema
      #+ FC.optional "transuranium" transuranium transuraniumSchema
      #+ FC.optional "gammaSeries" gammaSeries gammaSeriesSchema
      #+ FC.optional "omegaSeries" omegaSeries omegaSeriesSchema
      #+ FC.optional "worldSeries" worldSeries worldSeriesSchema