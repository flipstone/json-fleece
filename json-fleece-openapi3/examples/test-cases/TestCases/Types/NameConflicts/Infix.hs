{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.NameConflicts.Infix
  ( Infix(..)
  , infixSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Infix = Infix T.Text
  deriving (Show, Eq)

infixSchema :: FC.Fleece t => FC.Schema t Infix
infixSchema =
  FC.coerceSchema FC.text