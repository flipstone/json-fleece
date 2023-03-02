{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.NameConflicts.Module
  ( Module(..)
  , moduleSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Module = Module T.Text
  deriving (Show, Eq)

moduleSchema :: FC.Fleece schema => schema Module
moduleSchema =
  FC.coerceSchema FC.text