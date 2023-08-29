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
  { manufacturers :: Maybe [CompanyBase.CompanyBase] -- ^ Base company, returned in search results
  , releaseMonth :: Maybe ReleaseMonth.ReleaseMonth -- ^ Release month
  , tradingCards :: Maybe [TradingCardBase.TradingCardBase] -- ^ Base trading card, returned in search results
  , cardHeight :: Maybe CardHeight.CardHeight -- ^ Card height, in inches
  , packsPerBox :: Maybe PacksPerBox.PacksPerBox -- ^ Packs per box
  , productionRunUnit :: Maybe ProductionRunUnit.ProductionRunUnit -- ^ Production run unit
  , uid :: Uid.Uid -- ^ Trading card set unique ID
  , tradingCardDecks :: Maybe [TradingCardDeckBase.TradingCardDeckBase] -- ^ Base trading card deck, returned in search results
  , boxesPerCase :: Maybe BoxesPerCase.BoxesPerCase -- ^ Boxes per case
  , cardWidth :: Maybe CardWidth.CardWidth -- ^ Card width, in inches
  , countriesOfOrigin :: Maybe [Country.Country] -- ^ Real-world country
  , name :: Name.Name -- ^ Trading card set name
  , releaseYear :: Maybe ReleaseYear.ReleaseYear -- ^ Release year
  , cardsPerPack :: Maybe CardsPerPack.CardsPerPack -- ^ Cards per deck
  , productionRun :: Maybe ProductionRun.ProductionRun -- ^ Production run
  , releaseDay :: Maybe ReleaseDay.ReleaseDay -- ^ Release day
  }
  deriving (Eq, Show)

tradingCardSetFullSchema :: FC.Fleece schema => schema TradingCardSetFull
tradingCardSetFullSchema =
  FC.object $
    FC.constructor TradingCardSetFull
      #+ FC.optional "manufacturers" manufacturers (FC.list CompanyBase.companyBaseSchema)
      #+ FC.optional "releaseMonth" releaseMonth ReleaseMonth.releaseMonthSchema
      #+ FC.optional "tradingCards" tradingCards (FC.list TradingCardBase.tradingCardBaseSchema)
      #+ FC.optional "cardHeight" cardHeight CardHeight.cardHeightSchema
      #+ FC.optional "packsPerBox" packsPerBox PacksPerBox.packsPerBoxSchema
      #+ FC.optional "productionRunUnit" productionRunUnit ProductionRunUnit.productionRunUnitSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "tradingCardDecks" tradingCardDecks (FC.list TradingCardDeckBase.tradingCardDeckBaseSchema)
      #+ FC.optional "boxesPerCase" boxesPerCase BoxesPerCase.boxesPerCaseSchema
      #+ FC.optional "cardWidth" cardWidth CardWidth.cardWidthSchema
      #+ FC.optional "countriesOfOrigin" countriesOfOrigin (FC.list Country.countrySchema)
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "releaseYear" releaseYear ReleaseYear.releaseYearSchema
      #+ FC.optional "cardsPerPack" cardsPerPack CardsPerPack.cardsPerPackSchema
      #+ FC.optional "productionRun" productionRun ProductionRun.productionRunSchema
      #+ FC.optional "releaseDay" releaseDay ReleaseDay.releaseDaySchema