{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.NameConflicts.Data
  ( Data(..)
  , dataSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Data = Data T.Text
  deriving (Show, Eq)

dataSchema :: FC.Fleece schema => schema Data
dataSchema =
  FC.coerceSchema FC.text