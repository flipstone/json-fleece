{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.Element.Search.GET.PageNumber
  ( PageNumber(..)
  , paramDef
  ) where

import qualified Beeline.Params as P
import qualified Beeline.Routing as R
import qualified Data.Int as I
import Prelude (Eq, Show)

newtype PageNumber = PageNumber I.Int32
  deriving (Show, Eq)

paramDef :: R.ParameterDefinition PageNumber
paramDef =
  P.coerceParam (P.int32Param "pageNumber")