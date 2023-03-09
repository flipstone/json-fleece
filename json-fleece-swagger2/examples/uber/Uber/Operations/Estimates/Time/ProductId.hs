{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Uber.Operations.Estimates.Time.ProductId
  ( ProductId(..)
  , paramDef
  ) where

import qualified Beeline.Routing as R
import qualified Data.Text as T
import Prelude (Eq, Show)

newtype ProductId = ProductId T.Text
  deriving (Show, Eq)

paramDef :: R.ParameterDefinition ProductId
paramDef =
  R.coerceParam (R.textParam "product_id")