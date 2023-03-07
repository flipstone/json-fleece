{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.PerformerFullResponse
  ( PerformerFullResponse(..)
  , performerFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.PerformerFull as PerformerFull

data PerformerFullResponse = PerformerFullResponse
  { performer :: Maybe PerformerFull.PerformerFull -- ^ Full performer, returned when queried using UID
  }
  deriving (Eq, Show)

performerFullResponseSchema :: FC.Fleece schema => schema PerformerFullResponse
performerFullResponseSchema =
  FC.object $
    FC.constructor PerformerFullResponse
      #+ FC.optional "performer" performer PerformerFull.performerFullSchema