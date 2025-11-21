{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.InlineAllOf.Response200Body.Description
  ( Description(..)
  , descriptionSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Description = Description T.Text
  deriving (Show, Eq)

descriptionSchema :: FC.Fleece schema => schema Description
descriptionSchema =
  FC.coerceSchema FC.text