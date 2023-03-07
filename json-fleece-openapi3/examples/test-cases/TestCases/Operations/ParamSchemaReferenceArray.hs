{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.ParamSchemaReferenceArray
  ( ParamSchemaReferenceArray(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data ParamSchemaReferenceArray = ParamSchemaReferenceArray
  deriving (Eq, Show)

route :: R.Router r => r ParamSchemaReferenceArray
route =
  R.get $
    R.make ParamSchemaReferenceArray
      /- "test-cases"
      /- "param-ref-array"