{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.VideoReleaseHeader
  ( VideoReleaseHeader(..)
  , videoReleaseHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.Types.VideoReleaseHeader.Title as Title
import qualified StarTrek.Types.VideoReleaseHeader.Uid as Uid

data VideoReleaseHeader = VideoReleaseHeader
  { title :: Title.Title -- ^ Video release title
  , uid :: Uid.Uid -- ^ Video release unique ID
  }
  deriving (Eq, Show)

videoReleaseHeaderSchema :: FC.Fleece t => FC.Schema t VideoReleaseHeader
videoReleaseHeaderSchema =
  FC.object $
    FC.constructor VideoReleaseHeader
      #+ FC.required "title" title Title.titleSchema
      #+ FC.required "uid" uid Uid.uidSchema