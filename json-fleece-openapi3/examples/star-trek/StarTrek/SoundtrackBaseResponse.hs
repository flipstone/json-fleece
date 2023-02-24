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
      #+ FC.optionalField FC.OmitKey_DelegateNull "sort" sort responseSortSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "soundtracks" soundtracks (FC.list soundtrackBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "page" page responsePageSchema