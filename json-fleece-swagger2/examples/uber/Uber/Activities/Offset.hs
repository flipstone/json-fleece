{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Activities.Offset
  ( Offset(..)
  , offsetSchema
  ) where

import Data.Int (Int32)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Offset = Offset Int32
  deriving (Show, Eq)

offsetSchema :: FC.Fleece schema => schema Offset
offsetSchema =
  FC.coerceSchema FC.int32