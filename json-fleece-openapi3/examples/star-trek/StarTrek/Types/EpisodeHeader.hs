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
  { uid :: Uid.Uid -- ^ Episode unique ID
  , title :: Title.Title -- ^ Episode title
  }
  deriving (Eq, Show)

episodeHeaderSchema :: FC.Fleece schema => schema EpisodeHeader
episodeHeaderSchema =
  FC.object $
    FC.constructor EpisodeHeader
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.required "title" title Title.titleSchema