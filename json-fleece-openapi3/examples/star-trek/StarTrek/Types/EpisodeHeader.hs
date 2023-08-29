{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.EpisodeHeader
  ( EpisodeHeader(..)
  , episodeHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.Types.EpisodeHeader.Title as Title
import qualified StarTrek.Types.EpisodeHeader.Uid as Uid

data EpisodeHeader = EpisodeHeader
  { title :: Title.Title -- ^ Episode title
  , uid :: Uid.Uid -- ^ Episode unique ID
  }
  deriving (Eq, Show)

episodeHeaderSchema :: FC.Fleece schema => schema EpisodeHeader
episodeHeaderSchema =
  FC.object $
    FC.constructor EpisodeHeader
      #+ FC.required "title" title Title.titleSchema
      #+ FC.required "uid" uid Uid.uidSchema