{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicCollectionBase.NumberOfPages
  ( NumberOfPages(..)
  , numberOfPagesSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype NumberOfPages = NumberOfPages Integer
  deriving (Show, Eq)

numberOfPagesSchema :: FC.Fleece schema => schema NumberOfPages
numberOfPagesSchema =
  FC.coerceSchema FC.integer