{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.NameConflicts.Text
  ( Text(..)
  , textSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Text = Text T.Text
  deriving (Show, Eq)

textSchema :: FC.Fleece t => FC.Schema t Text
textSchema =
  FC.coerceSchema FC.text