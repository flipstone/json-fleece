{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.MedicalCondition.Uid
  ( Uid(..)
  , def
  ) where

import qualified Beeline.Routing as R
import qualified Data.Text as T
import Prelude (Eq, Show)

newtype Uid = Uid T.Text
  deriving (Show, Eq)

def :: R.ParameterDefinition Uid
def =
  R.coerceParam (R.textParam "uid")