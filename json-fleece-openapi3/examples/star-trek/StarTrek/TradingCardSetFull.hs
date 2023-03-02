{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardSetFull
  ( TradingCardSetFull(..)
  , tradingCardSetFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.CompanyBase as CompanyBase
import qualified StarTrek.Country as Country
import qualified StarTrek.ProductionRunUnit as ProductionRunUnit
import qualified StarTrek.TradingCardBase as TradingCardBase
import qualified StarTrek.TradingCardDeckBase as TradingCardDeckBase
import qualified StarTrek.TradingCardSetFull.BoxesPerCase as BoxesPerCase
import qualified StarTrek.TradingCardSetFull.CardHeight as CardHeight
import qualified StarTrek.TradingCardSetFull.CardWidth as CardWidth
import qualified StarTrek.TradingCardSetFull.CardsPerPack as CardsPerPack
import qualified StarTrek.TradingCardSetFull.Name as Name
import qualified StarTrek.TradingCardSetFull.PacksPerBox as PacksPerBox
import qualified StarTrek.TradingCardSetFull.ProductionRun as ProductionRun
import qualified StarTrek.TradingCardSetFull.ReleaseDay as ReleaseDay
import qualified StarTrek.TradingCardSetFull.ReleaseMonth as ReleaseMonth
import qualified StarTrek.TradingCardSetFull.ReleaseYear as ReleaseYear
import qualified StarTrek.TradingCardSetFull.Uid as Uid

data TradingCardSetFull = TradingCardSetFull
  { cardWidth :: Maybe CardWidth.CardWidth -- ^ Card width, in inches
  , name :: Name.Name -- ^ Trading card set name
  , packsPerBox :: Maybe PacksPerBox.PacksPerBox -- ^ Packs per box
  , releaseYear :: Maybe ReleaseYear.ReleaseYear -- ^ Release year
  , tradingCardDecks :: Maybe [TradingCardDeckBase.TradingCardDeckBase] -- ^ Base trading card deck, returned in search results
  , cardsPerPack :: Maybe CardsPerPack.CardsPerPack -- ^ Cards per deck
  , productionRunUnit :: Maybe ProductionRunUnit.ProductionRunUnit -- ^ Production run unit
  , countriesOfOrigin :: Maybe [Country.Country] -- ^ Real-world country
  , productionRun :: Maybe ProductionRun.ProductionRun -- ^ Production run
  , uid :: Uid.Uid -- ^ Trading card set unique ID
  , manufacturers :: Maybe [CompanyBase.CompanyBase] -- ^ Base company, returned in search results
  , tradingCards :: Maybe [TradingCardBase.TradingCardBase] -- ^ Base trading card, returned in search results
  , releaseMonth :: Maybe ReleaseMonth.ReleaseMonth -- ^ Release month
  , boxesPerCase :: Maybe BoxesPerCase.BoxesPerCase -- ^ Boxes per case
  , cardHeight :: Maybe CardHeight.CardHeight -- ^ Card height, in inches
  , releaseDay :: Maybe ReleaseDay.ReleaseDay -- ^ Release day
  }
  deriving (Eq, Show)

tradingCardSetFullSchema :: FC.Fleece schema => schema TradingCardSetFull
tradingCardSetFullSchema =
  FC.object $
    FC.constructor TradingCardSetFull
      #+ FC.optional "cardWidth" cardWidth CardWidth.cardWidthSchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "packsPerBox" packsPerBox PacksPerBox.packsPerBoxSchema
      #+ FC.optional "releaseYear" releaseYear ReleaseYear.releaseYearSchema
      #+ FC.optional "tradingCardDecks" tradingCardDecks (FC.list TradingCardDeckBase.tradingCardDeckBaseSchema)
      #+ FC.optional "cardsPerPack" cardsPerPack CardsPerPack.cardsPerPackSchema
      #+ FC.optional "productionRunUnit" productionRunUnit ProductionRunUnit.productionRunUnitSchema
      #+ FC.optional "countriesOfOrigin" countriesOfOrigin (FC.list Country.countrySchema)
      #+ FC.optional "productionRun" productionRun ProductionRun.productionRunSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "manufacturers" manufacturers (FC.list CompanyBase.companyBaseSchema)
      #+ FC.optional "tradingCards" tradingCards (FC.list TradingCardBase.tradingCardBaseSchema)
      #+ FC.optional "releaseMonth" releaseMonth ReleaseMonth.releaseMonthSchema
      #+ FC.optional "boxesPerCase" boxesPerCase BoxesPerCase.boxesPerCaseSchema
      #+ FC.optional "cardHeight" cardHeight CardHeight.cardHeightSchema
      #+ FC.optional "releaseDay" releaseDay ReleaseDay.releaseDaySchema