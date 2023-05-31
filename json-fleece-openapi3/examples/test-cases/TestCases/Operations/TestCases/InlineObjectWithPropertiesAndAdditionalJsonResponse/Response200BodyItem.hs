{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.TestCases.InlineObjectWithPropertiesAndAdditionalJsonResponse.Response200BodyItem
  ( Response200BodyItem(..)
  , response200BodyItemSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Response200BodyItem = Response200BodyItem T.Text
  deriving (Show, Eq)

response200BodyItemSchema :: FC.Fleece schema => schema Response200BodyItem
response200BodyItemSchema =
  FC.coerceSchema FC.text