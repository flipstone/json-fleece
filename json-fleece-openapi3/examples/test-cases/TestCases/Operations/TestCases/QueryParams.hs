{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.TestCases.QueryParams
  ( QueryParams(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data QueryParams = QueryParams
  deriving (Eq, Show)

route :: R.Router r => r QueryParams
route =
  R.get $
    R.make QueryParams
      /- "test-cases"
      /- "query-params"