{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.TestCases.InlineEnumArrayRequestBody.RequestBodyItem
  ( RequestBodyItem(..)
  , requestBodyItemSchema
  , requestBodyItemToText
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (($), Bounded, Enum, Eq, Ord, Show)

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

requestBodyItemSchema :: FC.Fleece schema => schema RequestBodyItem
requestBodyItemSchema =
  FC.boundedEnum requestBodyItemToText