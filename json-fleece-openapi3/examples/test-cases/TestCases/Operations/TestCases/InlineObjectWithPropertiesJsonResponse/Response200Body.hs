{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.TestCases.InlineObjectWithPropertiesJsonResponse.Response200Body
  ( Response200Body(..)
  , response200BodySchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Operations.TestCases.InlineObjectWithPropertiesJsonResponse.Response200Body.Bar as Bar
import qualified TestCases.Operations.TestCases.InlineObjectWithPropertiesJsonResponse.Response200Body.Baz as Baz
import qualified TestCases.Operations.TestCases.InlineObjectWithPropertiesJsonResponse.Response200Body.Foo as Foo

data Response200Body = Response200Body
  { baz :: Baz.Baz
  , bar :: Maybe Bar.Bar
  , foo :: Foo.Foo
  }
  deriving (Eq, Show)

response200BodySchema :: FC.Fleece schema => schema Response200Body
response200BodySchema =
  FC.object $
    FC.constructor Response200Body
      #+ FC.required "baz" baz Baz.bazSchema
      #+ FC.optional "bar" bar Bar.barSchema
      #+ FC.required "foo" foo Foo.fooSchema