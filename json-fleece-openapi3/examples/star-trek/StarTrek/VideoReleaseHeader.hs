{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoReleaseHeader
  ( VideoReleaseHeader(..)
  , videoReleaseHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import StarTrek.VideoReleaseHeader.Title (Title, titleSchema)
import StarTrek.VideoReleaseHeader.Uid (Uid, uidSchema)

data VideoReleaseHeader = VideoReleaseHeader
  { uid :: Uid -- ^ Video release unique ID
  , title :: Title -- ^ Video release title
  }
  deriving (Eq, Show)

videoReleaseHeaderSchema :: FC.Fleece schema => schema VideoReleaseHeader
videoReleaseHeaderSchema =
  FC.object $
    FC.constructor VideoReleaseHeader
      #+ FC.required "uid" uid uidSchema
      #+ FC.required "title" title titleSchema