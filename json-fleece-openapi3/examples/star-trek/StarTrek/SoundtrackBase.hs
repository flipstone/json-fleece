{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SoundtrackBase
  ( SoundtrackBase(..)
  , soundtrackBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.SoundtrackBase.Length as Length
import qualified StarTrek.SoundtrackBase.ReleaseDate as ReleaseDate
import qualified StarTrek.SoundtrackBase.Title as Title
import qualified StarTrek.SoundtrackBase.Uid as Uid

data SoundtrackBase = SoundtrackBase
  { releaseDate :: Maybe ReleaseDate.ReleaseDate -- ^ Release date
  , uid :: Uid.Uid -- ^ Soundtrack unique ID
  , length :: Maybe Length.Length -- ^ Length, in seconds
  , title :: Title.Title -- ^ Soundtrack title
  }
  deriving (Eq, Show)

soundtrackBaseSchema :: FC.Fleece schema => schema SoundtrackBase
soundtrackBaseSchema =
  FC.object $
    FC.constructor SoundtrackBase
      #+ FC.optional "releaseDate" releaseDate ReleaseDate.releaseDateSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "length" length Length.lengthSchema
      #+ FC.required "title" title Title.titleSchema