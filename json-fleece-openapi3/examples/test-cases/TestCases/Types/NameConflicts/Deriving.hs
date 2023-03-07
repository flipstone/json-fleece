{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.NameConflicts.Deriving
  ( Deriving(..)
  , derivingSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Deriving = Deriving T.Text
  deriving (Show, Eq)

derivingSchema :: FC.Fleece schema => schema Deriving
derivingSchema =
  FC.coerceSchema FC.text