{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SeasonFullResponse
  ( SeasonFullResponse(..)
  , seasonFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.SeasonFull (SeasonFull, seasonFullSchema)

data SeasonFullResponse = SeasonFullResponse
  { season :: Maybe SeasonFull -- ^ Full location, returned when queried using UID
  }
  deriving (Eq, Show)

seasonFullResponseSchema :: FC.Fleece schema => schema SeasonFullResponse
seasonFullResponseSchema =
  FC.object $
    FC.constructor SeasonFullResponse
      #+ FC.optional "season" season seasonFullSchema