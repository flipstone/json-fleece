{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.Series.Search.PageNumber
  ( PageNumber(..)
  , paramDef
  ) where

import qualified Beeline.Routing as R
import qualified Data.Int as I
import Prelude (Eq, Show)

newtype PageNumber = PageNumber I.Int32
  deriving (Show, Eq)

paramDef :: R.ParameterDefinition PageNumber
paramDef =
  R.coerceParam (R.int32Param "pageNumber")