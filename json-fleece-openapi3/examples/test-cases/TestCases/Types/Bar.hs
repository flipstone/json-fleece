{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.Bar
  ( Bar(..)
  , barSchema
  , barObjSchema
  ) where

import Fleece.Core ((#+), Object)
import qualified Fleece.Core as FC
import Prelude (Eq, Maybe, Show)
import qualified TestCases.Types.Bar.BarName as BarName

data Bar = Bar
  { barName :: Maybe BarName.BarName
  }
  deriving (Eq, Show)

barSchema :: FC.Fleece t => FC.Schema t Bar
barSchema =
  FC.object barObjSchema

barObjSchema :: FC.Fleece schema => Object schema Bar Bar
barObjSchema =
  FC.constructor Bar
    #+ FC.optional "barName" barName BarName.barNameSchema