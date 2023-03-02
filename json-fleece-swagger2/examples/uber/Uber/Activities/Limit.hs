{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Activities.Limit
  ( Limit(..)
  , limitSchema
  ) where

import qualified Data.Int as I
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Limit = Limit I.Int32
  deriving (Show, Eq)

limitSchema :: FC.Fleece schema => schema Limit
limitSchema =
  FC.coerceSchema FC.int32