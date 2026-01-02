{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.FieldDescriptions.WithDescription
  ( WithDescription(..)
  , withDescriptionSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype WithDescription = WithDescription T.Text
  deriving (Show, Eq)

withDescriptionSchema :: FC.Fleece t => FC.Schema t WithDescription
withDescriptionSchema =
  FC.coerceSchema FC.text