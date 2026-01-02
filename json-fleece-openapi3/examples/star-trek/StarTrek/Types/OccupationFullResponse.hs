{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.OccupationFullResponse
  ( OccupationFullResponse(..)
  , occupationFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.OccupationFull as OccupationFull

data OccupationFullResponse = OccupationFullResponse
  { occupation :: Maybe OccupationFull.OccupationFull -- ^ Full occupation, returned when queried using UID
  }
  deriving (Eq, Show)

occupationFullResponseSchema :: FC.Fleece t => FC.Schema t OccupationFullResponse
occupationFullResponseSchema =
  FC.object $
    FC.constructor OccupationFullResponse
      #+ FC.optional "occupation" occupation OccupationFull.occupationFullSchema