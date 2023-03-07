{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Types.Activities.Offset
  ( Offset(..)
  , offsetSchema
  ) where

import qualified Data.Int as I
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Offset = Offset I.Int32
  deriving (Show, Eq)

offsetSchema :: FC.Fleece schema => schema Offset
offsetSchema =
  FC.coerceSchema FC.int32