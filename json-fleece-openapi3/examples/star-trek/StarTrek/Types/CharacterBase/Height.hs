{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.CharacterBase.Height
  ( Height(..)
  , heightSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype Height = Height Integer
  deriving (Show, Eq)

heightSchema :: FC.Fleece t => FC.Schema t Height
heightSchema =
  FC.coerceSchema FC.integer