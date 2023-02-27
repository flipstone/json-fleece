{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoReleaseHeader
  ( VideoReleaseHeader(..)
  , videoReleaseHeaderSchema
  ) where

import Data.Text (Text)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)

data VideoReleaseHeader = VideoReleaseHeader
  { uid :: Text -- ^ Video release unique ID
  , title :: Text -- ^ Video release title
  }
  deriving (Eq, Show)

videoReleaseHeaderSchema :: FC.Fleece schema => schema VideoReleaseHeader
videoReleaseHeaderSchema =
  FC.object $
    FC.constructor VideoReleaseHeader
      #+ FC.required "uid" uid FC.text
      #+ FC.required "title" title FC.text