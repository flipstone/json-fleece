{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookSeriesHeader
  ( BookSeriesHeader(..)
  , bookSeriesHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.BookSeriesHeader.Title as Title
import qualified StarTrek.BookSeriesHeader.Uid as Uid

data BookSeriesHeader = BookSeriesHeader
  { uid :: Maybe Uid.Uid -- ^ Book series unique ID
  , title :: Maybe Title.Title -- ^ Book series title
  }
  deriving (Eq, Show)

bookSeriesHeaderSchema :: FC.Fleece schema => schema BookSeriesHeader
bookSeriesHeaderSchema =
  FC.object $
    FC.constructor BookSeriesHeader
      #+ FC.optional "uid" uid Uid.uidSchema
      #+ FC.optional "title" title Title.titleSchema