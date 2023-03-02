{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicSeriesBase.Miniseries
  ( Miniseries(..)
  , miniseriesSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Miniseries = Miniseries Bool
  deriving (Show, Eq)

miniseriesSchema :: FC.Fleece schema => schema Miniseries
miniseriesSchema =
  FC.coerceSchema FC.boolean