{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.NameConflicts.Int32
  ( Int32(..)
  , int32Schema
  ) where

import qualified Data.Int as I
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Int32 = Int32 I.Int32
  deriving (Show, Eq)

int32Schema :: FC.Fleece t => FC.Schema t Int32
int32Schema =
  FC.coerceSchema FC.int32