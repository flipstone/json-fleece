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

elementBaseSchema :: FC.Fleece schema => schema ElementBase
elementBaseSchema =
  FC.object $
    FC.constructor ElementBase
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