{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Types.Error.Code
  ( Code(..)
  , codeSchema
  ) where

import qualified Data.Int as I
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Code = Code I.Int32
  deriving (Show, Eq)

codeSchema :: FC.Fleece schema => schema Code
codeSchema =
  FC.coerceSchema FC.int32