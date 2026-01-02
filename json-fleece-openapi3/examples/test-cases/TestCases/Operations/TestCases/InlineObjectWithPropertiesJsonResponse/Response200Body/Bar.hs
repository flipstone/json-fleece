{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.TestCases.InlineObjectWithPropertiesJsonResponse.Response200Body.Bar
  ( Bar(..)
  , barSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Bar = Bar T.Text
  deriving (Show, Eq)

barSchema :: FC.Fleece t => FC.Schema t Bar
barSchema =
  FC.coerceSchema FC.text