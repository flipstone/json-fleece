{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.AstronomicalObjectFullResponse
  ( AstronomicalObjectFullResponse(..)
  , astronomicalObjectFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.AstronomicalObjectFull as AstronomicalObjectFull

data AstronomicalObjectFullResponse = AstronomicalObjectFullResponse
  { astronomicalObject :: Maybe AstronomicalObjectFull.AstronomicalObjectFull -- ^ Full astronomical object, returned when queried using UID
  }
  deriving (Eq, Show)

astronomicalObjectFullResponseSchema :: FC.Fleece t => FC.Schema t AstronomicalObjectFullResponse
astronomicalObjectFullResponseSchema =
  FC.object $
    FC.constructor AstronomicalObjectFullResponse
      #+ FC.optional "astronomicalObject" astronomicalObject AstronomicalObjectFull.astronomicalObjectFullSchema