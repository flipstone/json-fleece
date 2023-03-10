{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Uber.Operations.History.Limit
  ( Limit(..)
  , paramDef
  ) where

import qualified Beeline.Routing as R
import qualified Data.Int as I
import Prelude (Eq, Show)

newtype Limit = Limit I.Int32
  deriving (Show, Eq)

paramDef :: R.ParameterDefinition Limit
paramDef =
  R.coerceParam (R.int32Param "limit")