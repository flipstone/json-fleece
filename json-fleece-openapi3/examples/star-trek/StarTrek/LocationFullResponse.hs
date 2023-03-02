{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.LocationFullResponse
  ( LocationFullResponse(..)
  , locationFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.LocationFull as LocationFull

data LocationFullResponse = LocationFullResponse
  { location :: Maybe LocationFull.LocationFull -- ^ Full location, returned when queried using UID
  }
  deriving (Eq, Show)

locationFullResponseSchema :: FC.Fleece schema => schema LocationFullResponse
locationFullResponseSchema =
  FC.object $
    FC.constructor LocationFullResponse
      #+ FC.optional "location" location LocationFull.locationFullSchema