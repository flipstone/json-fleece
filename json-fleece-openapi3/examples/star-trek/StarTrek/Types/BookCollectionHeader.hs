{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.BookCollectionHeader
  ( BookCollectionHeader(..)
  , bookCollectionHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.BookCollectionHeader.Title as Title
import qualified StarTrek.Types.BookCollectionHeader.Uid as Uid

data BookCollectionHeader = BookCollectionHeader
  { uid :: Maybe Uid.Uid -- ^ Book collection unique ID
  , title :: Maybe Title.Title -- ^ Book collection title
  }
  deriving (Eq, Show)

bookCollectionHeaderSchema :: FC.Fleece schema => schema BookCollectionHeader
bookCollectionHeaderSchema =
  FC.object $
    FC.constructor BookCollectionHeader
      #+ FC.optional "uid" uid Uid.uidSchema
      #+ FC.optional "title" title Title.titleSchema