{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.NameConflicts.If
  ( If(..)
  , ifSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype If = If T.Text
  deriving (Show, Eq)

ifSchema :: FC.Fleece schema => schema If
ifSchema =
  FC.coerceSchema FC.text