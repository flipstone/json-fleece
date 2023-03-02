{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.OccupationFullResponse
  ( OccupationFullResponse(..)
  , occupationFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.OccupationFull as OccupationFull

data OccupationFullResponse = OccupationFullResponse
  { occupation :: Maybe OccupationFull.OccupationFull -- ^ Full occupation, returned when queried using UID
  }
  deriving (Eq, Show)

occupationFullResponseSchema :: FC.Fleece schema => schema OccupationFullResponse
occupationFullResponseSchema =
  FC.object $
    FC.constructor OccupationFullResponse
      #+ FC.optional "occupation" occupation OccupationFull.occupationFullSchema