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
  { atomicNumber :: Maybe AtomicNumber.AtomicNumber -- ^ Element atomic number
  , atomicWeight :: Maybe AtomicWeight.AtomicWeight -- ^ Element atomic weight
  , gammaSeries :: Maybe GammaSeries.GammaSeries -- ^ Whether it belongs to Gamma series
  , hypersonicSeries :: Maybe HypersonicSeries.HypersonicSeries -- ^ Whether it belongs to Hypersonic series
  , megaSeries :: Maybe MegaSeries.MegaSeries -- ^ Whether it belongs to Mega series
  , name :: Name.Name -- ^ Element name
  , omegaSeries :: Maybe OmegaSeries.OmegaSeries -- ^ Whether it belongs to Omega series
  , symbol :: Maybe Symbol.Symbol -- ^ Element symbol
  , transonicSeries :: Maybe TransonicSeries.TransonicSeries -- ^ Whether it belongs to Transonic series
  , transuranium :: Maybe Transuranium.Transuranium -- ^ Whether it's a transuranium
  , uid :: Uid.Uid -- ^ Element unique ID
  , worldSeries :: Maybe WorldSeries.WorldSeries -- ^ Whether it belongs to World series
  }
  deriving (Eq, Show)

elementFullSchema :: FC.Fleece schema => schema ElementFull
elementFullSchema =
  FC.object $
    FC.constructor ElementFull
      #+ FC.optional "atomicNumber" atomicNumber AtomicNumber.atomicNumberSchema
      #+ FC.optional "atomicWeight" atomicWeight AtomicWeight.atomicWeightSchema
      #+ FC.optional "gammaSeries" gammaSeries GammaSeries.gammaSeriesSchema
      #+ FC.optional "hypersonicSeries" hypersonicSeries HypersonicSeries.hypersonicSeriesSchema
      #+ FC.optional "megaSeries" megaSeries MegaSeries.megaSeriesSchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "omegaSeries" omegaSeries OmegaSeries.omegaSeriesSchema
      #+ FC.optional "symbol" symbol Symbol.symbolSchema
      #+ FC.optional "transonicSeries" transonicSeries TransonicSeries.transonicSeriesSchema
      #+ FC.optional "transuranium" transuranium Transuranium.transuraniumSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "worldSeries" worldSeries WorldSeries.worldSeriesSchema