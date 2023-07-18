{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SoundtrackBaseResponse
  ( SoundtrackBaseResponse(..)
  , soundtrackBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.ResponsePage as ResponsePage
import qualified StarTrek.Types.ResponseSort as ResponseSort
import qualified StarTrek.Types.SoundtrackBase as SoundtrackBase

data SoundtrackBaseResponse = SoundtrackBaseResponse
  { soundtracks :: Maybe [SoundtrackBase.SoundtrackBase] -- ^ Base soundtrack, returned in search results
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  }
  deriving (Eq, Show)

soundtrackBaseResponseSchema :: FC.Fleece schema => schema SoundtrackBaseResponse
soundtrackBaseResponseSchema =
  FC.object $
    FC.constructor SoundtrackBaseResponse
      #+ FC.optional "soundtracks" soundtracks (FC.list SoundtrackBase.soundtrackBaseSchema)
      #+ FC.optional "page" page ResponsePage.responsePageSchema
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema