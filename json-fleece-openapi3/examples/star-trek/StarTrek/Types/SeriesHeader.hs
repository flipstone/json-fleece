{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SeriesHeader
  ( SeriesHeader(..)
  , seriesHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.Types.SeriesHeader.Title as Title
import qualified StarTrek.Types.SeriesHeader.Uid as Uid

data SeriesHeader = SeriesHeader
  { uid :: Uid.Uid -- ^ Series unique ID
  , title :: Title.Title -- ^ Series title
  }
  deriving (Eq, Show)

seriesHeaderSchema :: FC.Fleece schema => schema SeriesHeader
seriesHeaderSchema =
  FC.object $
    FC.constructor SeriesHeader
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.required "title" title Title.titleSchema