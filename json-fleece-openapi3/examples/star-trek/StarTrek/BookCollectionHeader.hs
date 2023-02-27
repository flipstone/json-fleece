{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookCollectionHeader
  ( BookCollectionHeader(..)
  , bookCollectionHeaderSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)

data BookCollectionHeader = BookCollectionHeader
  { uid :: Maybe Text -- ^ Book collection unique ID
  , title :: Maybe Text -- ^ Book collection title
  }
  deriving (Eq, Show)

bookCollectionHeaderSchema :: FC.Fleece schema => schema BookCollectionHeader
bookCollectionHeaderSchema =
  FC.object $
    FC.constructor BookCollectionHeader
      #+ FC.optional "uid" uid FC.text
      #+ FC.optional "title" title FC.text