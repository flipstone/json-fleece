{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ElementFull
  ( ElementFull(..)
  , elementFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ElementFull.AtomicNumber (AtomicNumber, atomicNumberSchema)
import StarTrek.ElementFull.AtomicWeight (AtomicWeight, atomicWeightSchema)
import StarTrek.ElementFull.GammaSeries (GammaSeries, gammaSeriesSchema)
import StarTrek.ElementFull.HypersonicSeries (HypersonicSeries, hypersonicSeriesSchema)
import StarTrek.ElementFull.MegaSeries (MegaSeries, megaSeriesSchema)
import StarTrek.ElementFull.Name (Name, nameSchema)
import StarTrek.ElementFull.OmegaSeries (OmegaSeries, omegaSeriesSchema)
import StarTrek.ElementFull.Symbol (Symbol, symbolSchema)
import StarTrek.ElementFull.TransonicSeries (TransonicSeries, transonicSeriesSchema)
import StarTrek.ElementFull.Transuranium (Transuranium, transuraniumSchema)
import StarTrek.ElementFull.Uid (Uid, uidSchema)
import StarTrek.ElementFull.WorldSeries (WorldSeries, worldSeriesSchema)

data ElementFull = ElementFull
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

elementFullSchema :: FC.Fleece schema => schema ElementFull
elementFullSchema =
  FC.object $
    FC.constructor ElementFull
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