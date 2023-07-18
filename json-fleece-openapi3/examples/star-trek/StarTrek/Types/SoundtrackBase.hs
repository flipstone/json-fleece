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
  , uid :: Uid.Uid -- ^ Soundtrack unique ID
  , title :: Title.Title -- ^ Soundtrack title
  , releaseDate :: Maybe ReleaseDate.ReleaseDate -- ^ Release date
  }
  deriving (Eq, Show)

soundtrackBaseSchema :: FC.Fleece schema => schema SoundtrackBase
soundtrackBaseSchema =
  FC.object $
    FC.constructor SoundtrackBase
      #+ FC.optional "length" length Length.lengthSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "releaseDate" releaseDate ReleaseDate.releaseDateSchema