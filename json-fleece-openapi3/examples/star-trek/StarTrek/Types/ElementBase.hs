{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ElementBase
  ( ElementBase(..)
  , elementBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.ElementBase.AtomicNumber as AtomicNumber
import qualified StarTrek.Types.ElementBase.AtomicWeight as AtomicWeight
import qualified StarTrek.Types.ElementBase.GammaSeries as GammaSeries
import qualified StarTrek.Types.ElementBase.HypersonicSeries as HypersonicSeries
import qualified StarTrek.Types.ElementBase.MegaSeries as MegaSeries
import qualified StarTrek.Types.ElementBase.Name as Name
import qualified StarTrek.Types.ElementBase.OmegaSeries as OmegaSeries
import qualified StarTrek.Types.ElementBase.Symbol as Symbol
import qualified StarTrek.Types.ElementBase.TransonicSeries as TransonicSeries
import qualified StarTrek.Types.ElementBase.Transuranium as Transuranium
import qualified StarTrek.Types.ElementBase.Uid as Uid
import qualified StarTrek.Types.ElementBase.WorldSeries as WorldSeries

data ElementBase = ElementBase
  { omegaSeries :: Maybe OmegaSeries.OmegaSeries -- ^ Whether it belongs to Omega series
  , transuranium :: Maybe Transuranium.Transuranium -- ^ Whether it's a transuranium
  , gammaSeries :: Maybe GammaSeries.GammaSeries -- ^ Whether it belongs to Gamma series
  , symbol :: Maybe Symbol.Symbol -- ^ Element symbol
  , transonicSeries :: Maybe TransonicSeries.TransonicSeries -- ^ Whether it belongs to Transonic series
  , uid :: Uid.Uid -- ^ Element unique ID
  , hypersonicSeries :: Maybe HypersonicSeries.HypersonicSeries -- ^ Whether it belongs to Hypersonic series
  , atomicNumber :: Maybe AtomicNumber.AtomicNumber -- ^ Element atomic number
  , atomicWeight :: Maybe AtomicWeight.AtomicWeight -- ^ Element atomic weight
  , megaSeries :: Maybe MegaSeries.MegaSeries -- ^ Whether it belongs to Mega series
  , worldSeries :: Maybe WorldSeries.WorldSeries -- ^ Whether it belongs to World series
  , name :: Name.Name -- ^ Element name
  }
  deriving (Eq, Show)

elementBaseSchema :: FC.Fleece schema => schema ElementBase
elementBaseSchema =
  FC.object $
    FC.constructor ElementBase
      #+ FC.optional "omegaSeries" omegaSeries OmegaSeries.omegaSeriesSchema
      #+ FC.optional "transuranium" transuranium Transuranium.transuraniumSchema
      #+ FC.optional "gammaSeries" gammaSeries GammaSeries.gammaSeriesSchema
      #+ FC.optional "symbol" symbol Symbol.symbolSchema
      #+ FC.optional "transonicSeries" transonicSeries TransonicSeries.transonicSeriesSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "hypersonicSeries" hypersonicSeries HypersonicSeries.hypersonicSeriesSchema
      #+ FC.optional "atomicNumber" atomicNumber AtomicNumber.atomicNumberSchema
      #+ FC.optional "atomicWeight" atomicWeight AtomicWeight.atomicWeightSchema
      #+ FC.optional "megaSeries" megaSeries MegaSeries.megaSeriesSchema
      #+ FC.optional "worldSeries" worldSeries WorldSeries.worldSeriesSchema
      #+ FC.required "name" name Name.nameSchema