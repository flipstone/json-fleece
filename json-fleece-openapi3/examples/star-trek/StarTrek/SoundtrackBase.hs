{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SoundtrackBase
  ( SoundtrackBase(..)
  , soundtrackBaseSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Integer, Maybe, Show)

data SoundtrackBase = SoundtrackBase
  { releaseDate :: Maybe Text -- ^ Release date
  , uid :: Text -- ^ Soundtrack unique ID
  , length :: Maybe Integer -- ^ Length, in seconds
  , title :: Text -- ^ Soundtrack title
  }
  deriving (Eq, Show)

soundtrackBaseSchema :: FC.Fleece schema => schema SoundtrackBase
soundtrackBaseSchema =
  FC.object $
    FC.constructor SoundtrackBase
      #+ FC.optionalField FC.OmitKey_DelegateNull "releaseDate" releaseDate FC.text
      #+ FC.required "uid" uid FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "length" length FC.integer
      #+ FC.required "title" title FC.text