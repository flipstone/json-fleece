{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.IgnoredTextLengthType
  ( IgnoredTextLengthType(..)
  , ignoredTextLengthTypeSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype IgnoredTextLengthType = IgnoredTextLengthType T.Text
  deriving (Show, Eq)

ignoredTextLengthTypeSchema :: FC.Fleece t => FC.Schema t IgnoredTextLengthType
ignoredTextLengthTypeSchema =
  FC.coerceSchema FC.text