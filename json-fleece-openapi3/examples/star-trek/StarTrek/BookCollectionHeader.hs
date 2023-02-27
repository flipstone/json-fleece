{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookCollectionHeader
  ( BookCollectionHeader(..)
  , bookCollectionHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.BookCollectionHeader.Title (Title, titleSchema)
import StarTrek.BookCollectionHeader.Uid (Uid, uidSchema)

data BookCollectionHeader = BookCollectionHeader
  { uid :: Maybe Uid -- ^ Book collection unique ID
  , title :: Maybe Title -- ^ Book collection title
  }
  deriving (Eq, Show)

bookCollectionHeaderSchema :: FC.Fleece schema => schema BookCollectionHeader
bookCollectionHeaderSchema =
  FC.object $
    FC.constructor BookCollectionHeader
      #+ FC.optional "uid" uid uidSchema
      #+ FC.optional "title" title titleSchema