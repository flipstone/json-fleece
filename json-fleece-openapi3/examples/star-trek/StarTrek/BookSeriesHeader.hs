{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookSeriesHeader
  ( BookSeriesHeader(..)
  , bookSeriesHeaderSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)

data BookSeriesHeader = BookSeriesHeader
  { uid :: Maybe Text -- ^ Book series unique ID
  , title :: Maybe Text -- ^ Book series title
  }
  deriving (Eq, Show)

bookSeriesHeaderSchema :: FC.Fleece schema => schema BookSeriesHeader
bookSeriesHeaderSchema =
  FC.object $
    FC.constructor BookSeriesHeader
      #+ FC.optional "uid" uid FC.text
      #+ FC.optional "title" title FC.text