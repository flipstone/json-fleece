{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.Performer.Search.Sort
  ( Sort(..)
  , def
  ) where

import qualified Beeline.Routing as R
import qualified Data.Text as T
import Prelude (Eq, Show)

newtype Sort = Sort T.Text
  deriving (Show, Eq)

def :: R.ParameterDefinition Sort
def =
  R.coerceParam (R.textParam "sort")