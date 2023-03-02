{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.NameConflicts.NameConflicts
  ( NameConflicts(..)
  , nameConflictsSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype NameConflicts = NameConflicts T.Text
  deriving (Show, Eq)

nameConflictsSchema :: FC.Fleece schema => schema NameConflicts
nameConflictsSchema =
  FC.coerceSchema FC.text