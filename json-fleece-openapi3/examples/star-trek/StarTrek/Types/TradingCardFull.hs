{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TradingCardFull
  ( TradingCardFull(..)
  , tradingCardFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.TradingCardDeckBase as TradingCardDeckBase
import qualified StarTrek.Types.TradingCardFull.Name as Name
import qualified StarTrek.Types.TradingCardFull.Number as Number
import qualified StarTrek.Types.TradingCardFull.ProductionRun as ProductionRun
import qualified StarTrek.Types.TradingCardFull.ReleaseYear as ReleaseYear
import qualified StarTrek.Types.TradingCardFull.Uid as Uid
import qualified StarTrek.Types.TradingCardSetBase as TradingCardSetBase

data TradingCardFull = TradingCardFull
  { name :: Name.Name -- ^ Trading card name
  , number :: Maybe Number.Number -- ^ Trading card number
  , productionRun :: Maybe ProductionRun.ProductionRun -- ^ Production run, if different from trading card set production run
  , releaseYear :: Maybe ReleaseYear.ReleaseYear -- ^ Release year, if set was releases over multiple years
  , tradingCardDeck :: Maybe TradingCardDeckBase.TradingCardDeckBase -- ^ Base trading card deck, returned in search results
  , tradingCardSet :: Maybe TradingCardSetBase.TradingCardSetBase -- ^ Base trading card set, returned in search results
  , uid :: Uid.Uid -- ^ Trading card unique ID
  }
  deriving (Eq, Show)

tradingCardFullSchema :: FC.Fleece t => FC.Schema t TradingCardFull
tradingCardFullSchema =
  FC.object $
    FC.constructor TradingCardFull
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "number" number Number.numberSchema
      #+ FC.optional "productionRun" productionRun ProductionRun.productionRunSchema
      #+ FC.optional "releaseYear" releaseYear ReleaseYear.releaseYearSchema
      #+ FC.optional "tradingCardDeck" tradingCardDeck TradingCardDeckBase.tradingCardDeckBaseSchema
      #+ FC.optional "tradingCardSet" tradingCardSet TradingCardSetBase.tradingCardSetBaseSchema
      #+ FC.required "uid" uid Uid.uidSchema