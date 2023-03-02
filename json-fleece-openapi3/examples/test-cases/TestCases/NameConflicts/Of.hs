{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.NameConflicts.Of
  ( Of(..)
  , ofSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Of = Of T.Text
  deriving (Show, Eq)

ofSchema :: FC.Fleece schema => schema Of
ofSchema =
  FC.coerceSchema FC.text