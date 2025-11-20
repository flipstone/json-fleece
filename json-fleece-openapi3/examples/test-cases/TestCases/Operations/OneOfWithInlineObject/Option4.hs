{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.OneOfWithInlineObject.Option4
  ( Option4(..)
  , option4Schema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Operations.OneOfWithInlineObject.Option4.Nested as Nested

data Option4 = Option4
  { nested :: Maybe Nested.Nested
  }
  deriving (Eq, Show)

option4Schema :: FC.Fleece schema => schema Option4
option4Schema =
  FC.object $
    FC.constructor Option4
      #+ FC.optional "nested" nested Nested.nestedSchema