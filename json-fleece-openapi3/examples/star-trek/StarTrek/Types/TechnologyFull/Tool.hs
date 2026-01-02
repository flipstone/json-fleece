{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TechnologyFull.Tool
  ( Tool(..)
  , toolSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Tool = Tool Bool
  deriving (Show, Eq)

toolSchema :: FC.Fleece t => FC.Schema t Tool
toolSchema =
  FC.coerceSchema FC.boolean