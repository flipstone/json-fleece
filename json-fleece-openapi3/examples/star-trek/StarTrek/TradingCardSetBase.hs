{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardSetBase
  ( TradingCardSetBase(..)
  , tradingCardSetBaseSchema
  ) where

import qualified Fleece.Core as FC
import Data.Scientific (Scientific)
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Integer, Maybe, Show)
import StarTrek.ProductionRunUnit (ProductionRunUnit, productionRunUnitSchema)

data TradingCardSetBase = TradingCardSetBase
  { cardWidth :: Maybe Scientific -- ^ Card width, in inches
  , name :: Text -- ^ Trading card set name
  , packsPerBox :: Maybe Integer -- ^ Packs per box
  , releaseYear :: Maybe Integer -- ^ Release year
  , cardsPerPack :: Maybe Integer -- ^ Cards per deck
  , productionRunUnit :: Maybe ProductionRunUnit -- ^ Production run unit
  , productionRun :: Maybe Integer -- ^ Production run
  , uid :: Text -- ^ Trading card set unique ID
  , releaseMonth :: Maybe Integer -- ^ Release month
  , boxesPerCase :: Maybe Integer -- ^ Boxes per case
  , cardHeight :: Maybe Scientific -- ^ Card height, in inches
  , releaseDay :: Maybe Integer -- ^ Release day
  }
  deriving (Eq, Show)

tradingCardSetBaseSchema :: FC.Fleece schema => schema TradingCardSetBase
tradingCardSetBaseSchema =
  FC.object $
    FC.constructor TradingCardSetBase
      #+ FC.optional "cardWidth" cardWidth FC.number
      #+ FC.required "name" name FC.text
      #+ FC.optional "packsPerBox" packsPerBox FC.integer
      #+ FC.optional "releaseYear" releaseYear FC.integer
      #+ FC.optional "cardsPerPack" cardsPerPack FC.integer
      #+ FC.optional "productionRunUnit" productionRunUnit productionRunUnitSchema
      #+ FC.optional "productionRun" productionRun FC.integer
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "releaseMonth" releaseMonth FC.integer
      #+ FC.optional "boxesPerCase" boxesPerCase FC.integer
      #+ FC.optional "cardHeight" cardHeight FC.number
      #+ FC.optional "releaseDay" releaseDay FC.integer