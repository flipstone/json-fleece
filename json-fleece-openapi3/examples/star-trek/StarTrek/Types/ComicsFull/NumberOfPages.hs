{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ComicsFull.NumberOfPages
  ( NumberOfPages(..)
  , numberOfPagesSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype NumberOfPages = NumberOfPages Integer
  deriving (Show, Eq)

numberOfPagesSchema :: FC.Fleece schema => schema NumberOfPages
numberOfPagesSchema =
  FC.coerceSchema FC.integer