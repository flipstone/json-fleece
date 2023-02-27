{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardBase
  ( TradingCardBase(..)
  , tradingCardBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.TradingCardBase.Name (Name, nameSchema)
import StarTrek.TradingCardBase.Number (Number, numberSchema)
import StarTrek.TradingCardBase.ProductionRun (ProductionRun, productionRunSchema)
import StarTrek.TradingCardBase.ReleaseYear (ReleaseYear, releaseYearSchema)
import StarTrek.TradingCardBase.Uid (Uid, uidSchema)
import StarTrek.TradingCardDeckHeader (TradingCardDeckHeader, tradingCardDeckHeaderSchema)
import StarTrek.TradingCardSetHeader (TradingCardSetHeader, tradingCardSetHeaderSchema)

data TradingCardBase = TradingCardBase
  { number :: Maybe Number -- ^ Trading card number
  , tradingCardSet :: Maybe TradingCardSetHeader -- ^ Header trading card set, embedded in other objects
  , name :: Name -- ^ Trading card name
  , releaseYear :: Maybe ReleaseYear -- ^ Release year, if set was releases over multiple years
  , productionRun :: Maybe ProductionRun -- ^ Production run, if different from trading card set production run
  , uid :: Uid -- ^ Trading card unique ID
  , tradingCardDeck :: Maybe TradingCardDeckHeader -- ^ Header trading card deck, embedded in other objects
  }
  deriving (Eq, Show)

tradingCardBaseSchema :: FC.Fleece schema => schema TradingCardBase
tradingCardBaseSchema =
  FC.object $
    FC.constructor TradingCardBase
      #+ FC.optional "number" number numberSchema
      #+ FC.optional "tradingCardSet" tradingCardSet tradingCardSetHeaderSchema
      #+ FC.required "name" name nameSchema
      #+ FC.optional "releaseYear" releaseYear releaseYearSchema
      #+ FC.optional "productionRun" productionRun productionRunSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "tradingCardDeck" tradingCardDeck tradingCardDeckHeaderSchema