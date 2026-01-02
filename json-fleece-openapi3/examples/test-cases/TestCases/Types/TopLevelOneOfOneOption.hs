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

topLevelOneOfOneOptionSchema :: FC.Fleece t => FC.Schema t TopLevelOneOfOneOption
topLevelOneOfOneOptionSchema =
  FC.coerceSchema FC.text