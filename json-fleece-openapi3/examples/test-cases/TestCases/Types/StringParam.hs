{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.StringParam
  ( StringParam(..)
  , stringParamSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype StringParam = StringParam T.Text
  deriving (Show, Eq)

stringParamSchema :: FC.Fleece t => FC.Schema t StringParam
stringParamSchema =
  FC.coerceSchema FC.text