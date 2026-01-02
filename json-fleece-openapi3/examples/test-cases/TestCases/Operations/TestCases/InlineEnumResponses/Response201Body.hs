{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.TestCases.InlineEnumResponses.Response201Body
  ( Response201Body(..)
  , response201BodySchema
  , response201BodyToText
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (($), Bounded, Enum, Eq, Ord, Show)

data Response201Body
  = Baz
  | Bat
  deriving (Eq, Show, Ord, Enum, Bounded)

response201BodyToText :: Response201Body -> T.Text
response201BodyToText v =
  T.pack $
    case v of
      Baz -> "baz"
      Bat -> "bat"

response201BodySchema :: FC.Fleece t => FC.Schema t Response201Body
response201BodySchema =
  FC.boundedEnum response201BodyToText