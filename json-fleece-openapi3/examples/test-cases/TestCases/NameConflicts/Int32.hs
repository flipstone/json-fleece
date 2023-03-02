{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.NameConflicts.Int32
  ( Int32(..)
  , int32Schema
  ) where

import qualified Data.Int as I
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Int32 = Int32 I.Int32
  deriving (Show, Eq)

int32Schema :: FC.Fleece schema => schema Int32
int32Schema =
  FC.coerceSchema FC.int32