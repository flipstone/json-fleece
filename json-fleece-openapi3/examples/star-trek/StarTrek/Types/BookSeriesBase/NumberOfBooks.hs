{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.BookSeriesBase.NumberOfBooks
  ( NumberOfBooks(..)
  , numberOfBooksSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype NumberOfBooks = NumberOfBooks Integer
  deriving (Show, Eq)

numberOfBooksSchema :: FC.Fleece schema => schema NumberOfBooks
numberOfBooksSchema =
  FC.coerceSchema FC.integer