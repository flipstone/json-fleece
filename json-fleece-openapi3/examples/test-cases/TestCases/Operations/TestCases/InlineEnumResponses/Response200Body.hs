{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.TestCases.InlineEnumResponses.Response200Body
  ( Response200Body(..)
  , response200BodySchema
  , response200BodyToText
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (($), Bounded, Enum, Eq, Ord, Show)

data Response200Body
  = Foo
  | Bar
  deriving (Eq, Show, Ord, Enum, Bounded)

response200BodyToText :: Response200Body -> T.Text
response200BodyToText v =
  T.pack $
    case v of
      Foo -> "foo"
      Bar -> "bar"

response200BodySchema :: FC.Fleece t => FC.Schema t Response200Body
response200BodySchema =
  FC.boundedEnum response200BodyToText