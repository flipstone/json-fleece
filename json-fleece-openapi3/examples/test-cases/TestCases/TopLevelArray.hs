{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.TopLevelArray
  ( TopLevelArray(..)
  , topLevelArraySchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)
import TestCases.TopLevelArray.TopLevelArrayItem (TopLevelArrayItem, topLevelArrayItemSchema)

newtype TopLevelArray = TopLevelArray [TopLevelArrayItem]
  deriving (Show, Eq)

topLevelArraySchema :: FC.Fleece schema => schema TopLevelArray
topLevelArraySchema =
  FC.coerceSchema (FC.list topLevelArrayItemSchema)