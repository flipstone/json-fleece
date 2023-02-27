{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SoundtrackBase
  ( SoundtrackBase(..)
  , soundtrackBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.SoundtrackBase.Length (Length, lengthSchema)
import StarTrek.SoundtrackBase.ReleaseDate (ReleaseDate, releaseDateSchema)
import StarTrek.SoundtrackBase.Title (Title, titleSchema)
import StarTrek.SoundtrackBase.Uid (Uid, uidSchema)

data SoundtrackBase = SoundtrackBase
  { releaseDate :: Maybe ReleaseDate -- ^ Release date
  , uid :: Uid -- ^ Soundtrack unique ID
  , length :: Maybe Length -- ^ Length, in seconds
  , title :: Title -- ^ Soundtrack title
  }
  deriving (Eq, Show)

soundtrackBaseSchema :: FC.Fleece schema => schema SoundtrackBase
soundtrackBaseSchema =
  FC.object $
    FC.constructor SoundtrackBase
      #+ FC.optional "releaseDate" releaseDate releaseDateSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "length" length lengthSchema
      #+ FC.required "title" title titleSchema