{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.InheritedDiscriminatorChildB.ChildBName
  ( ChildBName(..)
  , childBNameSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype ChildBName = ChildBName T.Text
  deriving (Show, Eq)

childBNameSchema :: FC.Fleece t => FC.Schema t ChildBName
childBNameSchema =
  FC.coerceSchema FC.text