{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardFull
  ( TradingCardFull(..)
  , tradingCardFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.TradingCardDeckBase as TradingCardDeckBase
import qualified StarTrek.TradingCardFull.Name as Name
import qualified StarTrek.TradingCardFull.Number as Number
import qualified StarTrek.TradingCardFull.ProductionRun as ProductionRun
import qualified StarTrek.TradingCardFull.ReleaseYear as ReleaseYear
import qualified StarTrek.TradingCardFull.Uid as Uid
import qualified StarTrek.TradingCardSetBase as TradingCardSetBase

data TradingCardFull = TradingCardFull
  { number :: Maybe Number.Number -- ^ Trading card number
  , tradingCardSet :: Maybe TradingCardSetBase.TradingCardSetBase -- ^ Base trading card set, returned in search results
  , name :: Name.Name -- ^ Trading card name
  , releaseYear :: Maybe ReleaseYear.ReleaseYear -- ^ Release year, if set was releases over multiple years
  , productionRun :: Maybe ProductionRun.ProductionRun -- ^ Production run, if different from trading card set production run
  , uid :: Uid.Uid -- ^ Trading card unique ID
  , tradingCardDeck :: Maybe TradingCardDeckBase.TradingCardDeckBase -- ^ Base trading card deck, returned in search results
  }
  deriving (Eq, Show)

tradingCardFullSchema :: FC.Fleece schema => schema TradingCardFull
tradingCardFullSchema =
  FC.object $
    FC.constructor TradingCardFull
      #+ FC.optional "number" number Number.numberSchema
      #+ FC.optional "tradingCardSet" tradingCardSet TradingCardSetBase.tradingCardSetBaseSchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "releaseYear" releaseYear ReleaseYear.releaseYearSchema
      #+ FC.optional "productionRun" productionRun ProductionRun.productionRunSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "tradingCardDeck" tradingCardDeck TradingCardDeckBase.tradingCardDeckBaseSchema