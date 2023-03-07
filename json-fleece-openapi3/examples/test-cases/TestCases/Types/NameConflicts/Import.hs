{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.NameConflicts.Import
  ( Import(..)
  , importSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Import = Import T.Text
  deriving (Show, Eq)

importSchema :: FC.Fleece schema => schema Import
importSchema =
  FC.coerceSchema FC.text