{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffBase.ComicAuthor
  ( ComicAuthor(..)
  , comicAuthorSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ComicAuthor = ComicAuthor Bool
  deriving (Show, Eq)

comicAuthorSchema :: FC.Fleece schema => schema ComicAuthor
comicAuthorSchema =
  FC.coerceSchema FC.boolean