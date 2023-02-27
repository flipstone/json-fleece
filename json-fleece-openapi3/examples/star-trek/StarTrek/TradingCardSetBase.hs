{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardSetBase
  ( TradingCardSetBase(..)
  , tradingCardSetBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ProductionRunUnit (ProductionRunUnit, productionRunUnitSchema)
import StarTrek.TradingCardSetBase.BoxesPerCase (BoxesPerCase, boxesPerCaseSchema)
import StarTrek.TradingCardSetBase.CardHeight (CardHeight, cardHeightSchema)
import StarTrek.TradingCardSetBase.CardWidth (CardWidth, cardWidthSchema)
import StarTrek.TradingCardSetBase.CardsPerPack (CardsPerPack, cardsPerPackSchema)
import StarTrek.TradingCardSetBase.Name (Name, nameSchema)
import StarTrek.TradingCardSetBase.PacksPerBox (PacksPerBox, packsPerBoxSchema)
import StarTrek.TradingCardSetBase.ProductionRun (ProductionRun, productionRunSchema)
import StarTrek.TradingCardSetBase.ReleaseDay (ReleaseDay, releaseDaySchema)
import StarTrek.TradingCardSetBase.ReleaseMonth (ReleaseMonth, releaseMonthSchema)
import StarTrek.TradingCardSetBase.ReleaseYear (ReleaseYear, releaseYearSchema)
import StarTrek.TradingCardSetBase.Uid (Uid, uidSchema)

data TradingCardSetBase = TradingCardSetBase
  { cardWidth :: Maybe CardWidth -- ^ Card width, in inches
  , name :: Name -- ^ Trading card set name
  , packsPerBox :: Maybe PacksPerBox -- ^ Packs per box
  , releaseYear :: Maybe ReleaseYear -- ^ Release year
  , cardsPerPack :: Maybe CardsPerPack -- ^ Cards per deck
  , productionRunUnit :: Maybe ProductionRunUnit -- ^ Production run unit
  , productionRun :: Maybe ProductionRun -- ^ Production run
  , uid :: Uid -- ^ Trading card set unique ID
  , releaseMonth :: Maybe ReleaseMonth -- ^ Release month
  , boxesPerCase :: Maybe BoxesPerCase -- ^ Boxes per case
  , cardHeight :: Maybe CardHeight -- ^ Card height, in inches
  , releaseDay :: Maybe ReleaseDay -- ^ Release day
  }
  deriving (Eq, Show)

tradingCardSetBaseSchema :: FC.Fleece schema => schema TradingCardSetBase
tradingCardSetBaseSchema =
  FC.object $
    FC.constructor TradingCardSetBase
      #+ FC.optional "cardWidth" cardWidth cardWidthSchema
      #+ FC.required "name" name nameSchema
      #+ FC.optional "packsPerBox" packsPerBox packsPerBoxSchema
      #+ FC.optional "releaseYear" releaseYear releaseYearSchema
      #+ FC.optional "cardsPerPack" cardsPerPack cardsPerPackSchema
      #+ FC.optional "productionRunUnit" productionRunUnit productionRunUnitSchema
      #+ FC.optional "productionRun" productionRun productionRunSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "releaseMonth" releaseMonth releaseMonthSchema
      #+ FC.optional "boxesPerCase" boxesPerCase boxesPerCaseSchema
      #+ FC.optional "cardHeight" cardHeight cardHeightSchema
      #+ FC.optional "releaseDay" releaseDay releaseDaySchema