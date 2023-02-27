{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.TopLevelArray.TopLevelArrayItem
  ( TopLevelArrayItem(..)
  , topLevelArrayItemSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype TopLevelArrayItem = TopLevelArrayItem Text
  deriving (Show, Eq)

topLevelArrayItemSchema :: FC.Fleece schema => schema TopLevelArrayItem
topLevelArrayItemSchema =
  FC.coerceSchema FC.text