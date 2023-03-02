{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SoundtrackBaseResponse
  ( SoundtrackBaseResponse(..)
  , soundtrackBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.ResponsePage as ResponsePage
import qualified StarTrek.ResponseSort as ResponseSort
import qualified StarTrek.SoundtrackBase as SoundtrackBase

data SoundtrackBaseResponse = SoundtrackBaseResponse
  { sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  , soundtracks :: Maybe [SoundtrackBase.SoundtrackBase] -- ^ Base soundtrack, returned in search results
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

soundtrackBaseResponseSchema :: FC.Fleece schema => schema SoundtrackBaseResponse
soundtrackBaseResponseSchema =
  FC.object $
    FC.constructor SoundtrackBaseResponse
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema
      #+ FC.optional "soundtracks" soundtracks (FC.list SoundtrackBase.soundtrackBaseSchema)
      #+ FC.optional "page" page ResponsePage.responsePageSchema