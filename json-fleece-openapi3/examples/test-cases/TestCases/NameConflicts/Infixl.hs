{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.NameConflicts.Infixl
  ( Infixl(..)
  , infixlSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Infixl = Infixl T.Text
  deriving (Show, Eq)

infixlSchema :: FC.Fleece schema => schema Infixl
infixlSchema =
  FC.coerceSchema FC.text