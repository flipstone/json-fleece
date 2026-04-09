{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.TestCases.InlineEnumResponses.Response200Body
  ( Response200Body(..)
  , response200BodySchema
  , response200BodyToText
  , response200BodyFromText
  ) where

import qualified Data.Either as Either
import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (($), (<>), Bounded, Enum, Eq, Ord, Show, String)

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

response200BodyFromText :: T.Text -> Either.Either String Response200Body
response200BodyFromText txt =
  case T.unpack txt of
    "foo" -> Either.Right Foo
    "bar" -> Either.Right Bar
    v -> Either.Left $ "Unknown Response200Body: " <> v

response200BodySchema :: FC.Fleece t => FC.Schema t Response200Body
response200BodySchema =
  FC.boundedEnum response200BodyToText