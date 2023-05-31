{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.AStringType
  ( AStringType(..)
  , aStringTypeSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype AStringType = AStringType T.Text
  deriving (Show, Eq)

aStringTypeSchema :: FC.Fleece schema => schema AStringType
aStringTypeSchema =
  FC.coerceSchema FC.text