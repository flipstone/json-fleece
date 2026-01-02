{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.NameConflicts.Infixr
  ( Infixr(..)
  , infixrSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Infixr = Infixr T.Text
  deriving (Show, Eq)

infixrSchema :: FC.Fleece t => FC.Schema t Infixr
infixrSchema =
  FC.coerceSchema FC.text