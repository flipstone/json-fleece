{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.Bar
  ( Bar(..)
  , barSchema
  ) where

import Fleece.Core ((#+), Object)
import qualified Fleece.Core as FC
import Prelude (Eq, Maybe, Show)
import qualified TestCases.Types.Bar.BarName as BarName

data Bar = Bar
  { barName :: Maybe BarName.BarName
  }
  deriving (Eq, Show)

barSchema :: FC.Fleece schema => Object schema Bar Bar
barSchema =
  FC.constructor Bar
    #+ FC.optional "barName" barName BarName.barNameSchema