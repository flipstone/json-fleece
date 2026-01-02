{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.AnyOf.Option3.Id
  ( Id(..)
  , idSchema
  ) where

import qualified Data.Scientific as Sci
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Id = Id Sci.Scientific
  deriving (Show, Eq)

idSchema :: FC.Fleece t => FC.Schema t Id
idSchema =
  FC.coerceSchema FC.number