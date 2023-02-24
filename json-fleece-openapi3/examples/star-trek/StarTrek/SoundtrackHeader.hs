{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SoundtrackHeader
  ( SoundtrackHeader(..)
  , soundtrackHeaderSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Show)

data SoundtrackHeader = SoundtrackHeader
  { uid :: Text -- ^ Soundtrack unique ID
  , title :: Text -- ^ Soundtrack title
  }
  deriving (Eq, Show)

soundtrackHeaderSchema :: FC.Fleece schema => schema SoundtrackHeader
soundtrackHeaderSchema =
  FC.object $
    FC.constructor SoundtrackHeader
      #+ FC.required "uid" uid FC.text
      #+ FC.required "title" title FC.text