{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.NameConflicts.Where
  ( Where(..)
  , whereSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Where = Where T.Text
  deriving (Show, Eq)

whereSchema :: FC.Fleece schema => schema Where
whereSchema =
  FC.coerceSchema FC.text