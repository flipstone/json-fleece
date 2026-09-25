{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.PartiallyInheritedDiscriminatorChild.ChildName
  ( ChildName(..)
  , childNameSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype ChildName = ChildName T.Text
  deriving (Show, Eq)

childNameSchema :: FC.Fleece t => FC.Schema t ChildName
childNameSchema =
  FC.coerceSchema FC.text