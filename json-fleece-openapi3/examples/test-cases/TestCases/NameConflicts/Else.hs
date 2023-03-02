{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.NameConflicts.Else
  ( Else(..)
  , elseSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Else = Else T.Text
  deriving (Show, Eq)

elseSchema :: FC.Fleece schema => schema Else
elseSchema =
  FC.coerceSchema FC.text