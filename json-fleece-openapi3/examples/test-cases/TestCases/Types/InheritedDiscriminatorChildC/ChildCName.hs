{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.InheritedDiscriminatorChildC.ChildCName
  ( ChildCName(..)
  , childCNameSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype ChildCName = ChildCName T.Text
  deriving (Show, Eq)

childCNameSchema :: FC.Fleece t => FC.Schema t ChildCName
childCNameSchema =
  FC.coerceSchema FC.text