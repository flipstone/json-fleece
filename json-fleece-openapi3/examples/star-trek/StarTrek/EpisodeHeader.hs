{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.EpisodeHeader
  ( EpisodeHeader(..)
  , episodeHeaderSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Show)

data EpisodeHeader = EpisodeHeader
  { uid :: Text -- ^ Episode unique ID
  , title :: Text -- ^ Episode title
  }
  deriving (Eq, Show)

episodeHeaderSchema :: FC.Fleece schema => schema EpisodeHeader
episodeHeaderSchema =
  FC.object $
    FC.constructor EpisodeHeader
      #+ FC.required "uid" uid FC.text
      #+ FC.required "title" title FC.text