{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.InlineAllOf.Response400Body.Bad
  ( Bad(..)
  , badSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Bad = Bad T.Text
  deriving (Show, Eq)

badSchema :: FC.Fleece schema => schema Bad
badSchema =
  FC.coerceSchema FC.text