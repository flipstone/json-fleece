{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Uber.Operations.Estimates.Time.CustomerUuid
  ( CustomerUuid(..)
  , paramDef
  ) where

import qualified Beeline.Routing as R
import qualified Data.Text as T
import Prelude (Eq, Show)

newtype CustomerUuid = CustomerUuid T.Text
  deriving (Show, Eq)

paramDef :: R.ParameterDefinition CustomerUuid
paramDef =
  R.coerceParam (R.textParam "customer_uuid")