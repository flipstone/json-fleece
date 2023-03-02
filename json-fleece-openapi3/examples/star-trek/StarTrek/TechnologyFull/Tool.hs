{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TechnologyFull.Tool
  ( Tool(..)
  , toolSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Tool = Tool Bool
  deriving (Show, Eq)

toolSchema :: FC.Fleece schema => schema Tool
toolSchema =
  FC.coerceSchema FC.boolean