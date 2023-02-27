{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardFull
  ( TradingCardFull(..)
  , tradingCardFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.TradingCardDeckBase (TradingCardDeckBase, tradingCardDeckBaseSchema)
import StarTrek.TradingCardFull.Name (Name, nameSchema)
import StarTrek.TradingCardFull.Number (Number, numberSchema)
import StarTrek.TradingCardFull.ProductionRun (ProductionRun, productionRunSchema)
import StarTrek.TradingCardFull.ReleaseYear (ReleaseYear, releaseYearSchema)
import StarTrek.TradingCardFull.Uid (Uid, uidSchema)
import StarTrek.TradingCardSetBase (TradingCardSetBase, tradingCardSetBaseSchema)

data TradingCardFull = TradingCardFull
  { number :: Maybe Number -- ^ Trading card number
  , tradingCardSet :: Maybe TradingCardSetBase -- ^ Base trading card set, returned in search results
  , name :: Name -- ^ Trading card name
  , releaseYear :: Maybe ReleaseYear -- ^ Release year, if set was releases over multiple years
  , productionRun :: Maybe ProductionRun -- ^ Production run, if different from trading card set production run
  , uid :: Uid -- ^ Trading card unique ID
  , tradingCardDeck :: Maybe TradingCardDeckBase -- ^ Base trading card deck, returned in search results
  }
  deriving (Eq, Show)

tradingCardFullSchema :: FC.Fleece schema => schema TradingCardFull
tradingCardFullSchema =
  FC.object $
    FC.constructor TradingCardFull
      #+ FC.optional "number" number numberSchema
      #+ FC.optional "tradingCardSet" tradingCardSet tradingCardSetBaseSchema
      #+ FC.required "name" name nameSchema
      #+ FC.optional "releaseYear" releaseYear releaseYearSchema
      #+ FC.optional "productionRun" productionRun productionRunSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "tradingCardDeck" tradingCardDeck tradingCardDeckBaseSchema