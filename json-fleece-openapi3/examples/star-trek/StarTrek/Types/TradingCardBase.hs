{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TradingCardBase
  ( TradingCardBase(..)
  , tradingCardBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.TradingCardBase.Name as Name
import qualified StarTrek.Types.TradingCardBase.Number as Number
import qualified StarTrek.Types.TradingCardBase.ProductionRun as ProductionRun
import qualified StarTrek.Types.TradingCardBase.ReleaseYear as ReleaseYear
import qualified StarTrek.Types.TradingCardBase.Uid as Uid
import qualified StarTrek.Types.TradingCardDeckHeader as TradingCardDeckHeader
import qualified StarTrek.Types.TradingCardSetHeader as TradingCardSetHeader

data TradingCardBase = TradingCardBase
  { name :: Name.Name -- ^ Trading card name
  , number :: Maybe Number.Number -- ^ Trading card number
  , productionRun :: Maybe ProductionRun.ProductionRun -- ^ Production run, if different from trading card set production run
  , releaseYear :: Maybe ReleaseYear.ReleaseYear -- ^ Release year, if set was releases over multiple years
  , tradingCardDeck :: Maybe TradingCardDeckHeader.TradingCardDeckHeader -- ^ Header trading card deck, embedded in other objects
  , tradingCardSet :: Maybe TradingCardSetHeader.TradingCardSetHeader -- ^ Header trading card set, embedded in other objects
  , uid :: Uid.Uid -- ^ Trading card unique ID
  }
  deriving (Eq, Show)

tradingCardBaseSchema :: FC.Fleece t => FC.Schema t TradingCardBase
tradingCardBaseSchema =
  FC.object $
    FC.constructor TradingCardBase
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "number" number Number.numberSchema
      #+ FC.optional "productionRun" productionRun ProductionRun.productionRunSchema
      #+ FC.optional "releaseYear" releaseYear ReleaseYear.releaseYearSchema
      #+ FC.optional "tradingCardDeck" tradingCardDeck TradingCardDeckHeader.tradingCardDeckHeaderSchema
      #+ FC.optional "tradingCardSet" tradingCardSet TradingCardSetHeader.tradingCardSetHeaderSchema
      #+ FC.required "uid" uid Uid.uidSchema