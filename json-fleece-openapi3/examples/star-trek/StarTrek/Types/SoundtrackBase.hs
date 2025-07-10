{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SoundtrackBase
  ( SoundtrackBase(..)
  , soundtrackBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.SoundtrackBase.Length as Length
import qualified StarTrek.Types.SoundtrackBase.ReleaseDate as ReleaseDate
import qualified StarTrek.Types.SoundtrackBase.Title as Title
import qualified StarTrek.Types.SoundtrackBase.Uid as Uid

data SoundtrackBase = SoundtrackBase
  { length :: Maybe Length.Length -- ^ Length, in seconds
  , releaseDate :: Maybe ReleaseDate.ReleaseDate -- ^ Release date
  , title :: Title.Title -- ^ Soundtrack title
  , uid :: Uid.Uid -- ^ Soundtrack unique ID
  }
  deriving (Eq, Show)

soundtrackBaseSchema :: FC.Fleece schema => schema SoundtrackBase
soundtrackBaseSchema =
  FC.object $
    FC.constructor SoundtrackBase
      #+ FC.optional "length" length Length.lengthSchema
      #+ FC.optional "releaseDate" releaseDate ReleaseDate.releaseDateSchema
      #+ FC.required "title" title Title.titleSchema
      #+ FC.required "uid" uid Uid.uidSchema