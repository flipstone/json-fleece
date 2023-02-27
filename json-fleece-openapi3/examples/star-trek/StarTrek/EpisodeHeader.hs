{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.EpisodeHeader
  ( EpisodeHeader(..)
  , episodeHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import StarTrek.EpisodeHeader.Title (Title, titleSchema)
import StarTrek.EpisodeHeader.Uid (Uid, uidSchema)

data EpisodeHeader = EpisodeHeader
  { uid :: Uid -- ^ Episode unique ID
  , title :: Title -- ^ Episode title
  }
  deriving (Eq, Show)

episodeHeaderSchema :: FC.Fleece schema => schema EpisodeHeader
episodeHeaderSchema =
  FC.object $
    FC.constructor EpisodeHeader
      #+ FC.required "uid" uid uidSchema
      #+ FC.required "title" title titleSchema