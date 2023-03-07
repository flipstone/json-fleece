{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.TradingCardSet.Search.ApiKey
  ( ApiKey(..)
  , def
  ) where

import qualified Beeline.Routing as R
import qualified Data.Text as T
import Prelude (Eq, Show)

newtype ApiKey = ApiKey T.Text
  deriving (Show, Eq)

def :: R.ParameterDefinition ApiKey
def =
  R.coerceParam (R.textParam "apiKey")