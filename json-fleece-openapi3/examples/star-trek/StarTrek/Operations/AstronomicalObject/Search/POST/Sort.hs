{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.AstronomicalObject.Search.POST.Sort
  ( Sort(..)
  , paramDef
  ) where

import qualified Beeline.Params as P
import qualified Beeline.Routing as R
import qualified Data.Text as T
import Prelude (Eq, Show)

newtype Sort = Sort T.Text
  deriving (Show, Eq)

paramDef :: R.ParameterDefinition Sort
paramDef =
  P.coerceParam (P.textParam "sort")