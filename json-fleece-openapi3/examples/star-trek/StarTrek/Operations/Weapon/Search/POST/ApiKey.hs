{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.Weapon.Search.POST.ApiKey
  ( ApiKey(..)
  , paramDef
  ) where

import qualified Beeline.Params as P
import qualified Beeline.Routing as R
import qualified Data.Text as T
import Prelude (Eq, Show)

newtype ApiKey = ApiKey T.Text
  deriving (Show, Eq)

paramDef :: R.ParameterDefinition ApiKey
paramDef =
  P.coerceParam (P.textParam "apiKey")