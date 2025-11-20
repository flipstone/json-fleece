{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.OneOfWithInlineNestedArray.Option2ItemItem.Id
  ( Id(..)
  , idSchema
  ) where

import qualified Data.Scientific as Sci
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Id = Id Sci.Scientific
  deriving (Show, Eq)

idSchema :: FC.Fleece schema => schema Id
idSchema =
  FC.coerceSchema FC.number