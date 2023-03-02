{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardBase
  ( TradingCardBase(..)
  , tradingCardBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.TradingCardBase.Name as Name
import qualified StarTrek.TradingCardBase.Number as Number
import qualified StarTrek.TradingCardBase.ProductionRun as ProductionRun
import qualified StarTrek.TradingCardBase.ReleaseYear as ReleaseYear
import qualified StarTrek.TradingCardBase.Uid as Uid
import qualified StarTrek.TradingCardDeckHeader as TradingCardDeckHeader
import qualified StarTrek.TradingCardSetHeader as TradingCardSetHeader

data TradingCardBase = TradingCardBase
  { number :: Maybe Number.Number -- ^ Trading card number
  , tradingCardSet :: Maybe TradingCardSetHeader.TradingCardSetHeader -- ^ Header trading card set, embedded in other objects
  , name :: Name.Name -- ^ Trading card name
  , releaseYear :: Maybe ReleaseYear.ReleaseYear -- ^ Release year, if set was releases over multiple years
  , productionRun :: Maybe ProductionRun.ProductionRun -- ^ Production run, if different from trading card set production run
  , uid :: Uid.Uid -- ^ Trading card unique ID
  , tradingCardDeck :: Maybe TradingCardDeckHeader.TradingCardDeckHeader -- ^ Header trading card deck, embedded in other objects
  }
  deriving (Eq, Show)

tradingCardBaseSchema :: FC.Fleece schema => schema TradingCardBase
tradingCardBaseSchema =
  FC.object $
    FC.constructor TradingCardBase
      #+ FC.optional "number" number Number.numberSchema
      #+ FC.optional "tradingCardSet" tradingCardSet TradingCardSetHeader.tradingCardSetHeaderSchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "releaseYear" releaseYear ReleaseYear.releaseYearSchema
      #+ FC.optional "productionRun" productionRun ProductionRun.productionRunSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "tradingCardDeck" tradingCardDeck TradingCardDeckHeader.tradingCardDeckHeaderSchema