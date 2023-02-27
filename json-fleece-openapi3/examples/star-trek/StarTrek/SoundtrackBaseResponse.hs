{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SoundtrackBaseResponse
  ( SoundtrackBaseResponse(..)
  , soundtrackBaseResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)
import StarTrek.SoundtrackBase (SoundtrackBase, soundtrackBaseSchema)

data SoundtrackBaseResponse = SoundtrackBaseResponse
  { sort :: Maybe ResponseSort -- ^ Response sort
  , soundtracks :: Maybe [SoundtrackBase] -- ^ List of soundtracks matching given criteria
  , page :: Maybe ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

soundtrackBaseResponseSchema :: FC.Fleece schema => schema SoundtrackBaseResponse
soundtrackBaseResponseSchema =
  FC.object $
    FC.constructor SoundtrackBaseResponse
      #+ FC.optional "sort" sort responseSortSchema
      #+ FC.optional "soundtracks" soundtracks (FC.list soundtrackBaseSchema)
      #+ FC.optional "page" page responsePageSchema