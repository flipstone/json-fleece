{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Uber.Operations.Estimates.Time.CustomerUuid
  ( CustomerUuid(..)
  , paramDef
  ) where

import qualified Beeline.Params as P
import qualified Beeline.Routing as R
import qualified Data.Text as T
import Prelude (Eq, Show)

newtype CustomerUuid = CustomerUuid T.Text
  deriving (Show, Eq)

paramDef :: R.ParameterDefinition CustomerUuid
paramDef =
  P.coerceParam (P.textParam "customer_uuid")