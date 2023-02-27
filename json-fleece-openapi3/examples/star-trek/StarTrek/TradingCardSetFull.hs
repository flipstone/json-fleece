{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardSetFull
  ( TradingCardSetFull(..)
  , tradingCardSetFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.CompanyBase (CompanyBase, companyBaseSchema)
import StarTrek.Country (Country, countrySchema)
import StarTrek.ProductionRunUnit (ProductionRunUnit, productionRunUnitSchema)
import StarTrek.TradingCardBase (TradingCardBase, tradingCardBaseSchema)
import StarTrek.TradingCardDeckBase (TradingCardDeckBase, tradingCardDeckBaseSchema)
import StarTrek.TradingCardSetFull.BoxesPerCase (BoxesPerCase, boxesPerCaseSchema)
import StarTrek.TradingCardSetFull.CardHeight (CardHeight, cardHeightSchema)
import StarTrek.TradingCardSetFull.CardWidth (CardWidth, cardWidthSchema)
import StarTrek.TradingCardSetFull.CardsPerPack (CardsPerPack, cardsPerPackSchema)
import StarTrek.TradingCardSetFull.Name (Name, nameSchema)
import StarTrek.TradingCardSetFull.PacksPerBox (PacksPerBox, packsPerBoxSchema)
import StarTrek.TradingCardSetFull.ProductionRun (ProductionRun, productionRunSchema)
import StarTrek.TradingCardSetFull.ReleaseDay (ReleaseDay, releaseDaySchema)
import StarTrek.TradingCardSetFull.ReleaseMonth (ReleaseMonth, releaseMonthSchema)
import StarTrek.TradingCardSetFull.ReleaseYear (ReleaseYear, releaseYearSchema)
import StarTrek.TradingCardSetFull.Uid (Uid, uidSchema)

data TradingCardSetFull = TradingCardSetFull
  { cardWidth :: Maybe CardWidth -- ^ Card width, in inches
  , name :: Name -- ^ Trading card set name
  , packsPerBox :: Maybe PacksPerBox -- ^ Packs per box
  , releaseYear :: Maybe ReleaseYear -- ^ Release year
  , tradingCardDecks :: Maybe [TradingCardDeckBase] -- ^ Base trading card deck, returned in search results
  , cardsPerPack :: Maybe CardsPerPack -- ^ Cards per deck
  , productionRunUnit :: Maybe ProductionRunUnit -- ^ Production run unit
  , countriesOfOrigin :: Maybe [Country] -- ^ Real-world country
  , productionRun :: Maybe ProductionRun -- ^ Production run
  , uid :: Uid -- ^ Trading card set unique ID
  , manufacturers :: Maybe [CompanyBase] -- ^ Base company, returned in search results
  , tradingCards :: Maybe [TradingCardBase] -- ^ Base trading card, returned in search results
  , releaseMonth :: Maybe ReleaseMonth -- ^ Release month
  , boxesPerCase :: Maybe BoxesPerCase -- ^ Boxes per case
  , cardHeight :: Maybe CardHeight -- ^ Card height, in inches
  , releaseDay :: Maybe ReleaseDay -- ^ Release day
  }
  deriving (Eq, Show)

tradingCardSetFullSchema :: FC.Fleece schema => schema TradingCardSetFull
tradingCardSetFullSchema =
  FC.object $
    FC.constructor TradingCardSetFull
      #+ FC.optional "cardWidth" cardWidth cardWidthSchema
      #+ FC.required "name" name nameSchema
      #+ FC.optional "packsPerBox" packsPerBox packsPerBoxSchema
      #+ FC.optional "releaseYear" releaseYear releaseYearSchema
      #+ FC.optional "tradingCardDecks" tradingCardDecks (FC.list tradingCardDeckBaseSchema)
      #+ FC.optional "cardsPerPack" cardsPerPack cardsPerPackSchema
      #+ FC.optional "productionRunUnit" productionRunUnit productionRunUnitSchema
      #+ FC.optional "countriesOfOrigin" countriesOfOrigin (FC.list countrySchema)
      #+ FC.optional "productionRun" productionRun productionRunSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "manufacturers" manufacturers (FC.list companyBaseSchema)
      #+ FC.optional "tradingCards" tradingCards (FC.list tradingCardBaseSchema)
      #+ FC.optional "releaseMonth" releaseMonth releaseMonthSchema
      #+ FC.optional "boxesPerCase" boxesPerCase boxesPerCaseSchema
      #+ FC.optional "cardHeight" cardHeight cardHeightSchema
      #+ FC.optional "releaseDay" releaseDay releaseDaySchema