{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.ComicStrip.Search.POST.PageSize
  ( PageSize(..)
  , paramDef
  ) where

import qualified Beeline.Params as P
import qualified Beeline.Routing as R
import qualified Data.Int as I
import Prelude (Eq, Show)

newtype PageSize = PageSize I.Int32
  deriving (Show, Eq)

paramDef :: R.ParameterDefinition PageSize
paramDef =
  P.coerceParam (P.int32Param "pageSize")