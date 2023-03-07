{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.SpacecraftClass.Search.PageNumber
  ( PageNumber(..)
  , def
  ) where

import qualified Beeline.Routing as R
import qualified Data.Int as I
import Prelude (Eq, Show)

newtype PageNumber = PageNumber I.Int32
  deriving (Show, Eq)

def :: R.ParameterDefinition PageNumber
def =
  R.coerceParam (R.int32Param "pageNumber")