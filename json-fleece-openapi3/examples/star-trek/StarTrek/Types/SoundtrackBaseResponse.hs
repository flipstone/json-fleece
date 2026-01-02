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
  { page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  , soundtracks :: Maybe [SoundtrackBase.SoundtrackBase] -- ^ Base soundtrack, returned in search results
  }
  deriving (Eq, Show)

soundtrackBaseResponseSchema :: FC.Fleece t => FC.Schema t SoundtrackBaseResponse
soundtrackBaseResponseSchema =
  FC.object $
    FC.constructor SoundtrackBaseResponse
      #+ FC.optional "page" page ResponsePage.responsePageSchema
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema
      #+ FC.optional "soundtracks" soundtracks (FC.list SoundtrackBase.soundtrackBaseSchema)