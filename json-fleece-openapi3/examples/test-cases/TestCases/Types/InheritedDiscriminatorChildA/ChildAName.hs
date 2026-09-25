{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.InheritedDiscriminatorChildA.ChildAName
  ( ChildAName(..)
  , childANameSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype ChildAName = ChildAName T.Text
  deriving (Show, Eq)

childANameSchema :: FC.Fleece t => FC.Schema t ChildAName
childANameSchema =
  FC.coerceSchema FC.text