{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SeriesHeader
  ( SeriesHeader(..)
  , seriesHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import StarTrek.SeriesHeader.Title (Title, titleSchema)
import StarTrek.SeriesHeader.Uid (Uid, uidSchema)

data SeriesHeader = SeriesHeader
  { uid :: Uid -- ^ Series unique ID
  , title :: Title -- ^ Series title
  }
  deriving (Eq, Show)

seriesHeaderSchema :: FC.Fleece schema => schema SeriesHeader
seriesHeaderSchema =
  FC.object $
    FC.constructor SeriesHeader
      #+ FC.required "uid" uid uidSchema
      #+ FC.required "title" title titleSchema