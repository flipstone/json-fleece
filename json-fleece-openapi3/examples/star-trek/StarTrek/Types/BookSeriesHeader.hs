{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.BookSeriesHeader
  ( BookSeriesHeader(..)
  , bookSeriesHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.BookSeriesHeader.Title as Title
import qualified StarTrek.Types.BookSeriesHeader.Uid as Uid

data BookSeriesHeader = BookSeriesHeader
  { title :: Maybe Title.Title -- ^ Book series title
  , uid :: Maybe Uid.Uid -- ^ Book series unique ID
  }
  deriving (Eq, Show)

bookSeriesHeaderSchema :: FC.Fleece t => FC.Schema t BookSeriesHeader
bookSeriesHeaderSchema =
  FC.object $
    FC.constructor BookSeriesHeader
      #+ FC.optional "title" title Title.titleSchema
      #+ FC.optional "uid" uid Uid.uidSchema