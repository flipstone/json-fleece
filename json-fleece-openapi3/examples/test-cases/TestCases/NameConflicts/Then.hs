{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.NameConflicts.Then
  ( Then(..)
  , thenSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Then = Then T.Text
  deriving (Show, Eq)

thenSchema :: FC.Fleece schema => schema Then
thenSchema =
  FC.coerceSchema FC.text