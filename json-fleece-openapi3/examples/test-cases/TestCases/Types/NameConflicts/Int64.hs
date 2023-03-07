{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.NameConflicts.Int64
  ( Int64(..)
  , int64Schema
  ) where

import qualified Data.Int as I
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Int64 = Int64 I.Int64
  deriving (Show, Eq)

int64Schema :: FC.Fleece schema => schema Int64
int64Schema =
  FC.coerceSchema FC.int64