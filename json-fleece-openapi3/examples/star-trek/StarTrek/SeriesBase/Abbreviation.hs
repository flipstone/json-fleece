{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SeriesBase.Abbreviation
  ( Abbreviation(..)
  , abbreviationSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Abbreviation = Abbreviation Text
  deriving (Show, Eq)

abbreviationSchema :: FC.Fleece schema => schema Abbreviation
abbreviationSchema =
  FC.coerceSchema FC.text