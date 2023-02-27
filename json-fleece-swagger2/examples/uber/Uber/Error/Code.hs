{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Error.Code
  ( Code(..)
  , codeSchema
  ) where

import Data.Int (Int32)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Code = Code Int32
  deriving (Show, Eq)

codeSchema :: FC.Fleece schema => schema Code
codeSchema =
  FC.coerceSchema FC.int32