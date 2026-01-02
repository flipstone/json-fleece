{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.NameConflicts.Of
  ( Of(..)
  , ofSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Of = Of T.Text
  deriving (Show, Eq)

ofSchema :: FC.Fleece t => FC.Schema t Of
ofSchema =
  FC.coerceSchema FC.text