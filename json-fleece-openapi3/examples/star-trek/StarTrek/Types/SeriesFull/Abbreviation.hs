{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SeriesFull.Abbreviation
  ( Abbreviation(..)
  , abbreviationSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Abbreviation = Abbreviation T.Text
  deriving (Show, Eq)

abbreviationSchema :: FC.Fleece t => FC.Schema t Abbreviation
abbreviationSchema =
  FC.coerceSchema FC.text