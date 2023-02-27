{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.LocationFullResponse
  ( LocationFullResponse(..)
  , locationFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.LocationFull (LocationFull, locationFullSchema)

data LocationFullResponse = LocationFullResponse
  { location :: Maybe LocationFull -- ^ Full location, returned when queried using UID
  }
  deriving (Eq, Show)

locationFullResponseSchema :: FC.Fleece schema => schema LocationFullResponse
locationFullResponseSchema =
  FC.object $
    FC.constructor LocationFullResponse
      #+ FC.optional "location" location locationFullSchema