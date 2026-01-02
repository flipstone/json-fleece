{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SoundtrackFull.Length
  ( Length(..)
  , lengthSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype Length = Length Integer
  deriving (Show, Eq)

lengthSchema :: FC.Fleece t => FC.Schema t Length
lengthSchema =
  FC.coerceSchema FC.integer