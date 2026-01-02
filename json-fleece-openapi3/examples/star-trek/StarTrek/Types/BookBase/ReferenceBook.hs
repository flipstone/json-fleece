{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.BookBase.ReferenceBook
  ( ReferenceBook(..)
  , referenceBookSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ReferenceBook = ReferenceBook Bool
  deriving (Show, Eq)

referenceBookSchema :: FC.Fleece t => FC.Schema t ReferenceBook
referenceBookSchema =
  FC.coerceSchema FC.boolean