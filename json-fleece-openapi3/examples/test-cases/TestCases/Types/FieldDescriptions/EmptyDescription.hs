{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.FieldDescriptions.EmptyDescription
  ( EmptyDescription(..)
  , emptyDescriptionSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype EmptyDescription = EmptyDescription T.Text
  deriving (Show, Eq)

emptyDescriptionSchema :: FC.Fleece t => FC.Schema t EmptyDescription
emptyDescriptionSchema =
  FC.coerceSchema FC.text