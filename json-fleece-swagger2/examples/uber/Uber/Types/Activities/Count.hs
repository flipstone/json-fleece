{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Types.Activities.Count
  ( Count(..)
  , countSchema
  ) where

import qualified Data.Int as I
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Count = Count I.Int32
  deriving (Show, Eq)

countSchema :: FC.Fleece t => FC.Schema t Count
countSchema =
  FC.coerceSchema FC.int32