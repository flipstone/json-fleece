{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.Book.Search.PageSize
  ( PageSize(..)
  , paramDef
  ) where

import qualified Beeline.Routing as R
import qualified Data.Int as I
import Prelude (Eq, Show)

newtype PageSize = PageSize I.Int32
  deriving (Show, Eq)

paramDef :: R.ParameterDefinition PageSize
paramDef =
  R.coerceParam (R.int32Param "pageSize")