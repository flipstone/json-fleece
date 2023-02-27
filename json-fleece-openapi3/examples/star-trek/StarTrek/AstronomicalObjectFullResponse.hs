{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.AstronomicalObjectFullResponse
  ( AstronomicalObjectFullResponse(..)
  , astronomicalObjectFullResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.AstronomicalObjectFull (AstronomicalObjectFull, astronomicalObjectFullSchema)

data AstronomicalObjectFullResponse = AstronomicalObjectFullResponse
  { astronomicalObject :: Maybe AstronomicalObjectFull -- ^ Full astronomical object, returned when queried using UID
  }
  deriving (Eq, Show)

astronomicalObjectFullResponseSchema :: FC.Fleece schema => schema AstronomicalObjectFullResponse
astronomicalObjectFullResponseSchema =
  FC.object $
    FC.constructor AstronomicalObjectFullResponse
      #+ FC.optional "astronomicalObject" astronomicalObject astronomicalObjectFullSchema