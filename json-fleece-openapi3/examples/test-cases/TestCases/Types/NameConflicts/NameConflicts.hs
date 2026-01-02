{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.NameConflicts.NameConflicts
  ( NameConflicts(..)
  , nameConflictsSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype NameConflicts = NameConflicts T.Text
  deriving (Show, Eq)

nameConflictsSchema :: FC.Fleece t => FC.Schema t NameConflicts
nameConflictsSchema =
  FC.coerceSchema FC.text