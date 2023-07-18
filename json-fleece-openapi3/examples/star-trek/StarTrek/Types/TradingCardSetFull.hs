{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TradingCardSetFull
  ( TradingCardSetFull(..)
  , tradingCardSetFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.CompanyBase as CompanyBase
import qualified StarTrek.Types.Country as Country
import qualified StarTrek.Types.ProductionRunUnit as ProductionRunUnit
import qualified StarTrek.Types.TradingCardBase as TradingCardBase
import qualified StarTrek.Types.TradingCardDeckBase as TradingCardDeckBase
import qualified StarTrek.Types.TradingCardSetFull.BoxesPerCase as BoxesPerCase
import qualified StarTrek.Types.TradingCardSetFull.CardHeight as CardHeight
import qualified StarTrek.Types.TradingCardSetFull.CardWidth as CardWidth
import qualified StarTrek.Types.TradingCardSetFull.CardsPerPack as CardsPerPack
import qualified StarTrek.Types.TradingCardSetFull.Name as Name
import qualified StarTrek.Types.TradingCardSetFull.PacksPerBox as PacksPerBox
import qualified StarTrek.Types.TradingCardSetFull.ProductionRun as ProductionRun
import qualified StarTrek.Types.TradingCardSetFull.ReleaseDay as ReleaseDay
import qualified StarTrek.Types.TradingCardSetFull.ReleaseMonth as ReleaseMonth
import qualified StarTrek.Types.TradingCardSetFull.ReleaseYear as ReleaseYear
import qualified StarTrek.Types.TradingCardSetFull.Uid as Uid

data TradingCardSetFull = TradingCardSetFull
  { productionRunUnit :: Maybe ProductionRunUnit.ProductionRunUnit -- ^ Production run unit
  , releaseYear :: Maybe ReleaseYear.ReleaseYear -- ^ Release year
  , packsPerBox :: Maybe PacksPerBox.PacksPerBox -- ^ Packs per box
  , releaseMonth :: Maybe ReleaseMonth.ReleaseMonth -- ^ Release month
  , countriesOfOrigin :: Maybe [Country.Country] -- ^ Real-world country
  , manufacturers :: Maybe [CompanyBase.CompanyBase] -- ^ Base company, returned in search results
  , releaseDay :: Maybe ReleaseDay.ReleaseDay -- ^ Release day
  , uid :: Uid.Uid -- ^ Trading card set unique ID
  , cardHeight :: Maybe CardHeight.CardHeight -- ^ Card height, in inches
  , tradingCardDecks :: Maybe [TradingCardDeckBase.TradingCardDeckBase] -- ^ Base trading card deck, returned in search results
  , boxesPerCase :: Maybe BoxesPerCase.BoxesPerCase -- ^ Boxes per case
  , tradingCards :: Maybe [TradingCardBase.TradingCardBase] -- ^ Base trading card, returned in search results
  , cardWidth :: Maybe CardWidth.CardWidth -- ^ Card width, in inches
  , productionRun :: Maybe ProductionRun.ProductionRun -- ^ Production run
  , name :: Name.Name -- ^ Trading card set name
  , cardsPerPack :: Maybe CardsPerPack.CardsPerPack -- ^ Cards per deck
  }
  deriving (Eq, Show)

tradingCardSetFullSchema :: FC.Fleece schema => schema TradingCardSetFull
tradingCardSetFullSchema =
  FC.object $
    FC.constructor TradingCardSetFull
      #+ FC.optional "productionRunUnit" productionRunUnit ProductionRunUnit.productionRunUnitSchema
      #+ FC.optional "releaseYear" releaseYear ReleaseYear.releaseYearSchema
      #+ FC.optional "packsPerBox" packsPerBox PacksPerBox.packsPerBoxSchema
      #+ FC.optional "releaseMonth" releaseMonth ReleaseMonth.releaseMonthSchema
      #+ FC.optional "countriesOfOrigin" countriesOfOrigin (FC.list Country.countrySchema)
      #+ FC.optional "manufacturers" manufacturers (FC.list CompanyBase.companyBaseSchema)
      #+ FC.optional "releaseDay" releaseDay ReleaseDay.releaseDaySchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "cardHeight" cardHeight CardHeight.cardHeightSchema
      #+ FC.optional "tradingCardDecks" tradingCardDecks (FC.list TradingCardDeckBase.tradingCardDeckBaseSchema)
      #+ FC.optional "boxesPerCase" boxesPerCase BoxesPerCase.boxesPerCaseSchema
      #+ FC.optional "tradingCards" tradingCards (FC.list TradingCardBase.tradingCardBaseSchema)
      #+ FC.optional "cardWidth" cardWidth CardWidth.cardWidthSchema
      #+ FC.optional "productionRun" productionRun ProductionRun.productionRunSchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "cardsPerPack" cardsPerPack CardsPerPack.cardsPerPackSchema