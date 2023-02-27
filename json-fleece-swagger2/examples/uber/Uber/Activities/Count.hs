{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Activities.Count
  ( Count(..)
  , countSchema
  ) where

import Data.Int (Int32)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Count = Count Int32
  deriving (Show, Eq)

countSchema :: FC.Fleece schema => schema Count
countSchema =
  FC.coerceSchema FC.int32