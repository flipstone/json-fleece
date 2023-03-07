{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.MagazineSeries.Search.ApiKey
  ( ApiKey(..)
  , paramDef
  ) where

import qualified Beeline.Routing as R
import qualified Data.Text as T
import Prelude (Eq, Show)

newtype ApiKey = ApiKey T.Text
  deriving (Show, Eq)

paramDef :: R.ParameterDefinition ApiKey
paramDef =
  R.coerceParam (R.textParam "apiKey")