{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SeriesBase.Abbreviation
  ( Abbreviation(..)
  , abbreviationSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Abbreviation = Abbreviation T.Text
  deriving (Show, Eq)

abbreviationSchema :: FC.Fleece schema => schema Abbreviation
abbreviationSchema =
  FC.coerceSchema FC.text