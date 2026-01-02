{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ComicSeriesBase.Miniseries
  ( Miniseries(..)
  , miniseriesSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Miniseries = Miniseries Bool
  deriving (Show, Eq)

miniseriesSchema :: FC.Fleece t => FC.Schema t Miniseries
miniseriesSchema =
  FC.coerceSchema FC.boolean