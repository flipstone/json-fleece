{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.Book.Uid
  ( Uid(..)
  , paramDef
  ) where

import qualified Beeline.Routing as R
import qualified Data.Text as T
import Prelude (Eq, Show)

newtype Uid = Uid T.Text
  deriving (Show, Eq)

paramDef :: R.ParameterDefinition Uid
paramDef =
  R.coerceParam (R.textParam "uid")