{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.PartiallyInheritedDiscriminatorNonChild.NonChildName
  ( NonChildName(..)
  , nonChildNameSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype NonChildName = NonChildName T.Text
  deriving (Show, Eq)

nonChildNameSchema :: FC.Fleece t => FC.Schema t NonChildName
nonChildNameSchema =
  FC.coerceSchema FC.text