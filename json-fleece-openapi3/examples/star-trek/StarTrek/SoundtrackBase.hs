{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SoundtrackBase
  ( SoundtrackBase(..)
  , soundtrackBaseSchema
  ) where

import Data.Text (Text)
import Data.Time (Day)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Integer, Maybe, Show)

data SoundtrackBase = SoundtrackBase
  { releaseDate :: Maybe Day -- ^ Release date
  , uid :: Text -- ^ Soundtrack unique ID
  , length :: Maybe Integer -- ^ Length, in seconds
  , title :: Text -- ^ Soundtrack title
  }
  deriving (Eq, Show)

soundtrackBaseSchema :: FC.Fleece schema => schema SoundtrackBase
soundtrackBaseSchema =
  FC.object $
    FC.constructor SoundtrackBase
      #+ FC.optional "releaseDate" releaseDate FC.day
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "length" length FC.integer
      #+ FC.required "title" title FC.text