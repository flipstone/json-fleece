{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module TestCases.Operations.BoundedTextPathParam.BoundedParam
  ( BoundedParam(..)
  , paramDef
  ) where

import qualified Beeline.Params as P
import qualified Beeline.Routing as R
import qualified Data.BoundedText as BT
import Prelude (Eq, Show)

newtype BoundedParam = BoundedParam (BT.BoundedText 1 100)
  deriving (Show, Eq)

paramDef :: R.ParameterDefinition BoundedParam
paramDef =
  P.coerceParam (P.boundedTextParam "bounded-param")