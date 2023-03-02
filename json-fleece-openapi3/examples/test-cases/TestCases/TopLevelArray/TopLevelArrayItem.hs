{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.TopLevelArray.TopLevelArrayItem
  ( TopLevelArrayItem(..)
  , topLevelArrayItemSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype TopLevelArrayItem = TopLevelArrayItem T.Text
  deriving (Show, Eq)

topLevelArrayItemSchema :: FC.Fleece schema => schema TopLevelArrayItem
topLevelArrayItemSchema =
  FC.coerceSchema FC.text