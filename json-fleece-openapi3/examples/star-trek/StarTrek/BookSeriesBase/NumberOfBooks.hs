{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookSeriesBase.NumberOfBooks
  ( NumberOfBooks(..)
  , numberOfBooksSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype NumberOfBooks = NumberOfBooks Integer
  deriving (Show, Eq)

numberOfBooksSchema :: FC.Fleece schema => schema NumberOfBooks
numberOfBooksSchema =
  FC.coerceSchema FC.integer