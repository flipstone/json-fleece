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

importSchema :: FC.Fleece t => FC.Schema t Import
importSchema =
  FC.coerceSchema FC.text