{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.TestCases.InlineEnumArrayRequestBody.RequestBodyItem
  ( RequestBodyItem(..)
  , requestBodyItemSchema
  , requestBodyItemToText
  , requestBodyItemFromText
  ) where

import qualified Data.Either as Either
import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (($), (<>), Bounded, Either, Enum, Eq, Ord, Show, String)

data RequestBodyItem
  = Foo
  | Bar
  | Baz
  deriving (Eq, Show, Ord, Enum, Bounded)

requestBodyItemToText :: RequestBodyItem -> T.Text
requestBodyItemToText v =
  T.pack $
    case v of
      Foo -> "foo"
      Bar -> "bar"
      Baz -> "baz"

requestBodyItemFromText :: T.Text -> Either String RequestBodyItem
requestBodyItemFromText txt =
  case T.unpack txt of
    "foo" -> Either.Right Foo
    "bar" -> Either.Right Bar
    "baz" -> Either.Right Baz
    v -> Either.Left $ "Unknown RequestBodyItem: " <> v

requestBodyItemSchema :: FC.Fleece t => FC.Schema t RequestBodyItem
requestBodyItemSchema =
  FC.boundedEnum requestBodyItemToText