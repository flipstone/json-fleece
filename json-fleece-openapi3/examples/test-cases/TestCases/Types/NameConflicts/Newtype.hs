{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.NameConflicts.Newtype
  ( Newtype(..)
  , newtypeSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Newtype = Newtype T.Text
  deriving (Show, Eq)

newtypeSchema :: FC.Fleece schema => schema Newtype
newtypeSchema =
  FC.coerceSchema FC.text