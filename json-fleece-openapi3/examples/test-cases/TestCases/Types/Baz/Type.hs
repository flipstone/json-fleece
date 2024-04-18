{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.Baz.Type
  ( Type(..)
  , typeSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Type = Type T.Text
  deriving (Show, Eq)

typeSchema :: FC.Fleece schema => schema Type
typeSchema =
  FC.coerceSchema FC.text