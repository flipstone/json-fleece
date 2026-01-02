{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.AnyOf.Option3.Description
  ( Description(..)
  , descriptionSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Description = Description T.Text
  deriving (Show, Eq)

descriptionSchema :: FC.Fleece t => FC.Schema t Description
descriptionSchema =
  FC.coerceSchema FC.text