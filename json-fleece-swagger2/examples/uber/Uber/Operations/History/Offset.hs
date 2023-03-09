{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Uber.Operations.History.Offset
  ( Offset(..)
  , paramDef
  ) where

import qualified Beeline.Routing as R
import qualified Data.Int as I
import Prelude (Eq, Show)

newtype Offset = Offset I.Int32
  deriving (Show, Eq)

paramDef :: R.ParameterDefinition Offset
paramDef =
  R.coerceParam (R.int32Param "offset")