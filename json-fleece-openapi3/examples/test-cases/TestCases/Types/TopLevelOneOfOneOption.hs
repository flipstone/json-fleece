{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.TopLevelOneOfOneOption
  ( TopLevelOneOfOneOption(..)
  , topLevelOneOfOneOptionSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype TopLevelOneOfOneOption = TopLevelOneOfOneOption T.Text
  deriving (Show, Eq)

topLevelOneOfOneOptionSchema :: FC.Fleece schema => schema TopLevelOneOfOneOption
topLevelOneOfOneOptionSchema =
  FC.coerceSchema FC.text