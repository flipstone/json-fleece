{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.NameConflicts.Case
  ( Case(..)
  , caseSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Case = Case T.Text
  deriving (Show, Eq)

caseSchema :: FC.Fleece schema => schema Case
caseSchema =
  FC.coerceSchema FC.text