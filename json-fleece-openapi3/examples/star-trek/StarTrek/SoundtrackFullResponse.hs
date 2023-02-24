{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SoundtrackFullResponse
  ( SoundtrackFullResponse(..)
  , soundtrackFullResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.SoundtrackFull (SoundtrackFull, soundtrackFullSchema)

data SoundtrackFullResponse = SoundtrackFullResponse
  { soundtrack :: Maybe SoundtrackFull -- ^ Full soundtrack, returned when queried using UID
  }
  deriving (Eq, Show)

soundtrackFullResponseSchema :: FC.Fleece schema => schema SoundtrackFullResponse
soundtrackFullResponseSchema =
  FC.object $
    FC.constructor SoundtrackFullResponse
      #+ FC.optionalField FC.OmitKey_DelegateNull "soundtrack" soundtrack soundtrackFullSchema