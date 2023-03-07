{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TradingCardSetBase
  ( TradingCardSetBase(..)
  , tradingCardSetBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.ProductionRunUnit as ProductionRunUnit
import qualified StarTrek.Types.TradingCardSetBase.BoxesPerCase as BoxesPerCase
import qualified StarTrek.Types.TradingCardSetBase.CardHeight as CardHeight
import qualified StarTrek.Types.TradingCardSetBase.CardWidth as CardWidth
import qualified StarTrek.Types.TradingCardSetBase.CardsPerPack as CardsPerPack
import qualified StarTrek.Types.TradingCardSetBase.Name as Name
import qualified StarTrek.Types.TradingCardSetBase.PacksPerBox as PacksPerBox
import qualified StarTrek.Types.TradingCardSetBase.ProductionRun as ProductionRun
import qualified StarTrek.Types.TradingCardSetBase.ReleaseDay as ReleaseDay
import qualified StarTrek.Types.TradingCardSetBase.ReleaseMonth as ReleaseMonth
import qualified StarTrek.Types.TradingCardSetBase.ReleaseYear as ReleaseYear
import qualified StarTrek.Types.TradingCardSetBase.Uid as Uid

data TradingCardSetBase = TradingCardSetBase
  { cardWidth :: Maybe CardWidth.CardWidth -- ^ Card width, in inches
  , name :: Name.Name -- ^ Trading card set name
  , packsPerBox :: Maybe PacksPerBox.PacksPerBox -- ^ Packs per box
  , releaseYear :: Maybe ReleaseYear.ReleaseYear -- ^ Release year
  , cardsPerPack :: Maybe CardsPerPack.CardsPerPack -- ^ Cards per deck
  , productionRunUnit :: Maybe ProductionRunUnit.ProductionRunUnit -- ^ Production run unit
  , productionRun :: Maybe ProductionRun.ProductionRun -- ^ Production run
  , uid :: Uid.Uid -- ^ Trading card set unique ID
  , releaseMonth :: Maybe ReleaseMonth.ReleaseMonth -- ^ Release month
  , boxesPerCase :: Maybe BoxesPerCase.BoxesPerCase -- ^ Boxes per case
  , cardHeight :: Maybe CardHeight.CardHeight -- ^ Card height, in inches
  , releaseDay :: Maybe ReleaseDay.ReleaseDay -- ^ Release day
  }
  deriving (Eq, Show)

tradingCardSetBaseSchema :: FC.Fleece schema => schema TradingCardSetBase
tradingCardSetBaseSchema =
  FC.object $
    FC.constructor TradingCardSetBase
      #+ FC.optional "cardWidth" cardWidth CardWidth.cardWidthSchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "packsPerBox" packsPerBox PacksPerBox.packsPerBoxSchema
      #+ FC.optional "releaseYear" releaseYear ReleaseYear.releaseYearSchema
      #+ FC.optional "cardsPerPack" cardsPerPack CardsPerPack.cardsPerPackSchema
      #+ FC.optional "productionRunUnit" productionRunUnit ProductionRunUnit.productionRunUnitSchema
      #+ FC.optional "productionRun" productionRun ProductionRun.productionRunSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "releaseMonth" releaseMonth ReleaseMonth.releaseMonthSchema
      #+ FC.optional "boxesPerCase" boxesPerCase BoxesPerCase.boxesPerCaseSchema
      #+ FC.optional "cardHeight" cardHeight CardHeight.cardHeightSchema
      #+ FC.optional "releaseDay" releaseDay ReleaseDay.releaseDaySchema