{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ComicsBase.NumberOfPages
  ( NumberOfPages(..)
  , numberOfPagesSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype NumberOfPages = NumberOfPages Integer
  deriving (Show, Eq)

numberOfPagesSchema :: FC.Fleece t => FC.Schema t NumberOfPages
numberOfPagesSchema =
  FC.coerceSchema FC.integer