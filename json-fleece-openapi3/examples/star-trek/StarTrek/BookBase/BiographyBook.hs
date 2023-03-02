{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookBase.BiographyBook
  ( BiographyBook(..)
  , biographyBookSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype BiographyBook = BiographyBook Bool
  deriving (Show, Eq)

biographyBookSchema :: FC.Fleece schema => schema BiographyBook
biographyBookSchema =
  FC.coerceSchema FC.boolean