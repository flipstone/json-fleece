{-# LANGUAGE NoImplicitPrelude #-}

module OneOfTest.Types.Obj.Foobar
  ( Foobar(..)
  , foobarSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype Foobar = Foobar Integer
  deriving (Show, Eq)

foobarSchema :: FC.Fleece schema => schema Foobar
foobarSchema =
  FC.coerceSchema FC.integer