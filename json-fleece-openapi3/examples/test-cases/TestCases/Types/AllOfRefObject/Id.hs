{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.AllOfRefObject.Id
  ( Id(..)
  , idSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype Id = Id Integer
  deriving (Show, Eq)

idSchema :: FC.Fleece schema => schema Id
idSchema =
  FC.coerceSchema FC.integer