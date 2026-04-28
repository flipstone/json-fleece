{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}

module TestCases.Types.BoundedTextType
  ( BoundedTextType(..)
  , boundedTextTypeSchema
  ) where

import qualified Data.BoundedText as BT
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype BoundedTextType = BoundedTextType (BT.BoundedText 1 100)
  deriving (Show, Eq)

boundedTextTypeSchema :: FC.Fleece t => FC.Schema t BoundedTextType
boundedTextTypeSchema =
  FC.coerceSchema FC.boundedText