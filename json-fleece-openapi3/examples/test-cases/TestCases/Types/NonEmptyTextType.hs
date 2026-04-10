{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.NonEmptyTextType
  ( NonEmptyTextType(..)
  , nonEmptyTextTypeSchema
  ) where

import qualified Data.NonEmptyText as NET
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype NonEmptyTextType = NonEmptyTextType NET.NonEmptyText
  deriving (Show, Eq)

nonEmptyTextTypeSchema :: FC.Fleece t => FC.Schema t NonEmptyTextType
nonEmptyTextTypeSchema =
  FC.coerceSchema FC.nonEmptyText
