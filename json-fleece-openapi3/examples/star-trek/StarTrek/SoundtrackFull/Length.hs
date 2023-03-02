{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SoundtrackFull.Length
  ( Length(..)
  , lengthSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype Length = Length Integer
  deriving (Show, Eq)

lengthSchema :: FC.Fleece schema => schema Length
lengthSchema =
  FC.coerceSchema FC.integer