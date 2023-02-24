{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.OccupationFullResponse
  ( OccupationFullResponse(..)
  , occupationFullResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.OccupationFull (OccupationFull, occupationFullSchema)

data OccupationFullResponse = OccupationFullResponse
  { occupation :: Maybe OccupationFull -- ^ Full occupation, returned when queried using UID
  }
  deriving (Eq, Show)

occupationFullResponseSchema :: FC.Fleece schema => schema OccupationFullResponse
occupationFullResponseSchema =
  FC.object $
    FC.constructor OccupationFullResponse
      #+ FC.optionalField FC.OmitKey_DelegateNull "occupation" occupation occupationFullSchema