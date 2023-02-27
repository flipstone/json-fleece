{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookSeriesHeader
  ( BookSeriesHeader(..)
  , bookSeriesHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.BookSeriesHeader.Title (Title, titleSchema)
import StarTrek.BookSeriesHeader.Uid (Uid, uidSchema)

data BookSeriesHeader = BookSeriesHeader
  { uid :: Maybe Uid -- ^ Book series unique ID
  , title :: Maybe Title -- ^ Book series title
  }
  deriving (Eq, Show)

bookSeriesHeaderSchema :: FC.Fleece schema => schema BookSeriesHeader
bookSeriesHeaderSchema =
  FC.object $
    FC.constructor BookSeriesHeader
      #+ FC.optional "uid" uid uidSchema
      #+ FC.optional "title" title titleSchema