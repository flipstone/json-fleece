{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.MedicalCondition.Search.PageSize
  ( PageSize(..)
  , def
  ) where

import qualified Beeline.Routing as R
import qualified Data.Int as I
import Prelude (Eq, Show)

newtype PageSize = PageSize I.Int32
  deriving (Show, Eq)

def :: R.ParameterDefinition PageSize
def =
  R.coerceParam (R.int32Param "pageSize")