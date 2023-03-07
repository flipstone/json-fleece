{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SoundtrackFullResponse
  ( SoundtrackFullResponse(..)
  , soundtrackFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.SoundtrackFull as SoundtrackFull

data SoundtrackFullResponse = SoundtrackFullResponse
  { soundtrack :: Maybe SoundtrackFull.SoundtrackFull -- ^ Full soundtrack, returned when queried using UID
  }
  deriving (Eq, Show)

soundtrackFullResponseSchema :: FC.Fleece schema => schema SoundtrackFullResponse
soundtrackFullResponseSchema =
  FC.object $
    FC.constructor SoundtrackFullResponse
      #+ FC.optional "soundtrack" soundtrack SoundtrackFull.soundtrackFullSchema