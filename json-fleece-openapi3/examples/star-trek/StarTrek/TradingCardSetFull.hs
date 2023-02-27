{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardSetFull
  ( TradingCardSetFull(..)
  , tradingCardSetFullSchema
  ) where

import qualified Fleece.Core as FC
import Data.Scientific (Scientific)
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Integer, Maybe, Show)
import StarTrek.CompanyBase (CompanyBase, companyBaseSchema)
import StarTrek.Country (Country, countrySchema)
import StarTrek.ProductionRunUnit (ProductionRunUnit, productionRunUnitSchema)
import StarTrek.TradingCardBase (TradingCardBase, tradingCardBaseSchema)
import StarTrek.TradingCardDeckBase (TradingCardDeckBase, tradingCardDeckBaseSchema)

data TradingCardSetFull = TradingCardSetFull
  { cardWidth :: Maybe Scientific -- ^ Card width, in inches
  , name :: Text -- ^ Trading card set name
  , packsPerBox :: Maybe Integer -- ^ Packs per box
  , releaseYear :: Maybe Integer -- ^ Release year
  , tradingCardDecks :: Maybe [TradingCardDeckBase] -- ^ Trading card decks in this set
  , cardsPerPack :: Maybe Integer -- ^ Cards per deck
  , productionRunUnit :: Maybe ProductionRunUnit -- ^ Production run unit
  , countriesOfOrigin :: Maybe [Country] -- ^ Countries of origin
  , productionRun :: Maybe Integer -- ^ Production run
  , uid :: Text -- ^ Trading card set unique ID
  , manufacturers :: Maybe [CompanyBase] -- ^ Manufacturers
  , tradingCards :: Maybe [TradingCardBase] -- ^ Trading cards in this set
  , releaseMonth :: Maybe Integer -- ^ Release month
  , boxesPerCase :: Maybe Integer -- ^ Boxes per case
  , cardHeight :: Maybe Scientific -- ^ Card height, in inches
  , releaseDay :: Maybe Integer -- ^ Release day
  }
  deriving (Eq, Show)

tradingCardSetFullSchema :: FC.Fleece schema => schema TradingCardSetFull
tradingCardSetFullSchema =
  FC.object $
    FC.constructor TradingCardSetFull
      #+ FC.optional "cardWidth" cardWidth FC.number
      #+ FC.required "name" name FC.text
      #+ FC.optional "packsPerBox" packsPerBox FC.integer
      #+ FC.optional "releaseYear" releaseYear FC.integer
      #+ FC.optional "tradingCardDecks" tradingCardDecks (FC.list tradingCardDeckBaseSchema)
      #+ FC.optional "cardsPerPack" cardsPerPack FC.integer
      #+ FC.optional "productionRunUnit" productionRunUnit productionRunUnitSchema
      #+ FC.optional "countriesOfOrigin" countriesOfOrigin (FC.list countrySchema)
      #+ FC.optional "productionRun" productionRun FC.integer
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "manufacturers" manufacturers (FC.list companyBaseSchema)
      #+ FC.optional "tradingCards" tradingCards (FC.list tradingCardBaseSchema)
      #+ FC.optional "releaseMonth" releaseMonth FC.integer
      #+ FC.optional "boxesPerCase" boxesPerCase FC.integer
      #+ FC.optional "cardHeight" cardHeight FC.number
      #+ FC.optional "releaseDay" releaseDay FC.integer