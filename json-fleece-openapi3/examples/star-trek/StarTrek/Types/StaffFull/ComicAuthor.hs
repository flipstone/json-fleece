{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffFull.ComicAuthor
  ( ComicAuthor(..)
  , comicAuthorSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ComicAuthor = ComicAuthor Bool
  deriving (Show, Eq)

comicAuthorSchema :: FC.Fleece t => FC.Schema t ComicAuthor
comicAuthorSchema =
  FC.coerceSchema FC.boolean