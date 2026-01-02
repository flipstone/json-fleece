{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.BookFull.BiographyBook
  ( BiographyBook(..)
  , biographyBookSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype BiographyBook = BiographyBook Bool
  deriving (Show, Eq)

biographyBookSchema :: FC.Fleece t => FC.Schema t BiographyBook
biographyBookSchema =
  FC.coerceSchema FC.boolean