{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.AstronomicalObjectFullResponse
  ( AstronomicalObjectFullResponse(..)
  , astronomicalObjectFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.AstronomicalObjectFull as AstronomicalObjectFull

data AstronomicalObjectFullResponse = AstronomicalObjectFullResponse
  { astronomicalObject :: Maybe AstronomicalObjectFull.AstronomicalObjectFull -- ^ Full astronomical object, returned when queried using UID
  }
  deriving (Eq, Show)

astronomicalObjectFullResponseSchema :: FC.Fleece schema => schema AstronomicalObjectFullResponse
astronomicalObjectFullResponseSchema =
  FC.object $
    FC.constructor AstronomicalObjectFullResponse
      #+ FC.optional "astronomicalObject" astronomicalObject AstronomicalObjectFull.astronomicalObjectFullSchema