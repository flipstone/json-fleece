{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.PerformerFullResponse
  ( PerformerFullResponse(..)
  , performerFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.PerformerFull (PerformerFull, performerFullSchema)

data PerformerFullResponse = PerformerFullResponse
  { performer :: Maybe PerformerFull -- ^ Full performer, returned when queried using UID
  }
  deriving (Eq, Show)

performerFullResponseSchema :: FC.Fleece schema => schema PerformerFullResponse
performerFullResponseSchema =
  FC.object $
    FC.constructor PerformerFullResponse
      #+ FC.optional "performer" performer performerFullSchema