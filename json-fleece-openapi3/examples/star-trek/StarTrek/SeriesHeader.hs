{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SeriesHeader
  ( SeriesHeader(..)
  , seriesHeaderSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Show)

data SeriesHeader = SeriesHeader
  { uid :: Text -- ^ Series unique ID
  , title :: Text -- ^ Series title
  }
  deriving (Eq, Show)

seriesHeaderSchema :: FC.Fleece schema => schema SeriesHeader
seriesHeaderSchema =
  FC.object $
    FC.constructor SeriesHeader
      #+ FC.required "uid" uid FC.text
      #+ FC.required "title" title FC.text