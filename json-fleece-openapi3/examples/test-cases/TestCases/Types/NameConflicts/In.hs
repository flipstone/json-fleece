{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.NameConflicts.In
  ( In(..)
  , inSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype In = In T.Text
  deriving (Show, Eq)

inSchema :: FC.Fleece t => FC.Schema t In
inSchema =
  FC.coerceSchema FC.text