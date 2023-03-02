{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookFull.ReferenceBook
  ( ReferenceBook(..)
  , referenceBookSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ReferenceBook = ReferenceBook Bool
  deriving (Show, Eq)

referenceBookSchema :: FC.Fleece schema => schema ReferenceBook
referenceBookSchema =
  FC.coerceSchema FC.boolean