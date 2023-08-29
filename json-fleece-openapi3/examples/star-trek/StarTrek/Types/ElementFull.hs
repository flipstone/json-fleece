{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ElementFull
  ( ElementFull(..)
  , elementFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.ElementFull.AtomicNumber as AtomicNumber
import qualified StarTrek.Types.ElementFull.AtomicWeight as AtomicWeight
import qualified StarTrek.Types.ElementFull.GammaSeries as GammaSeries
import qualified StarTrek.Types.ElementFull.HypersonicSeries as HypersonicSeries
import qualified StarTrek.Types.ElementFull.MegaSeries as MegaSeries
import qualified StarTrek.Types.ElementFull.Name as Name
import qualified StarTrek.Types.ElementFull.OmegaSeries as OmegaSeries
import qualified StarTrek.Types.ElementFull.Symbol as Symbol
import qualified StarTrek.Types.ElementFull.TransonicSeries as TransonicSeries
import qualified StarTrek.Types.ElementFull.Transuranium as Transuranium
import qualified StarTrek.Types.ElementFull.Uid as Uid
import qualified StarTrek.Types.ElementFull.WorldSeries as WorldSeries

data ElementFull = ElementFull
  { atomicWeight :: Maybe AtomicWeight.AtomicWeight -- ^ Element atomic weight
  , hypersonicSeries :: Maybe HypersonicSeries.HypersonicSeries -- ^ Whether it belongs to Hypersonic series
  , worldSeries :: Maybe WorldSeries.WorldSeries -- ^ Whether it belongs to World series
  , atomicNumber :: Maybe AtomicNumber.AtomicNumber -- ^ Element atomic number
  , transonicSeries :: Maybe TransonicSeries.TransonicSeries -- ^ Whether it belongs to Transonic series
  , symbol :: Maybe Symbol.Symbol -- ^ Element symbol
  , uid :: Uid.Uid -- ^ Element unique ID
  , transuranium :: Maybe Transuranium.Transuranium -- ^ Whether it's a transuranium
  , omegaSeries :: Maybe OmegaSeries.OmegaSeries -- ^ Whether it belongs to Omega series
  , megaSeries :: Maybe MegaSeries.MegaSeries -- ^ Whether it belongs to Mega series
  , name :: Name.Name -- ^ Element name
  , gammaSeries :: Maybe GammaSeries.GammaSeries -- ^ Whether it belongs to Gamma series
  }
  deriving (Eq, Show)

elementFullSchema :: FC.Fleece schema => schema ElementFull
elementFullSchema =
  FC.object $
    FC.constructor ElementFull
      #+ FC.optional "atomicWeight" atomicWeight AtomicWeight.atomicWeightSchema
      #+ FC.optional "hypersonicSeries" hypersonicSeries HypersonicSeries.hypersonicSeriesSchema
      #+ FC.optional "worldSeries" worldSeries WorldSeries.worldSeriesSchema
      #+ FC.optional "atomicNumber" atomicNumber AtomicNumber.atomicNumberSchema
      #+ FC.optional "transonicSeries" transonicSeries TransonicSeries.transonicSeriesSchema
      #+ FC.optional "symbol" symbol Symbol.symbolSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "transuranium" transuranium Transuranium.transuraniumSchema
      #+ FC.optional "omegaSeries" omegaSeries OmegaSeries.omegaSeriesSchema
      #+ FC.optional "megaSeries" megaSeries MegaSeries.megaSeriesSchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "gammaSeries" gammaSeries GammaSeries.gammaSeriesSchema