{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CharacterFull.Height
  ( Height(..)
  , heightSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype Height = Height Integer
  deriving (Show, Eq)

heightSchema :: FC.Fleece schema => schema Height
heightSchema =
  FC.coerceSchema FC.integer