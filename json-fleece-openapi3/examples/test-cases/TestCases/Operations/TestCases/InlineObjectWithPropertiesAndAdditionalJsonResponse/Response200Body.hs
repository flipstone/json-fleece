{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.TestCases.InlineObjectWithPropertiesAndAdditionalJsonResponse.Response200Body
  ( Response200Body(..)
  , response200BodySchema
  ) where

import qualified Data.Map as Map
import qualified Data.Text as T
import Fleece.Core ((#*), (#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Operations.TestCases.InlineObjectWithPropertiesAndAdditionalJsonResponse.Response200Body.Bar as Bar
import qualified TestCases.Operations.TestCases.InlineObjectWithPropertiesAndAdditionalJsonResponse.Response200Body.Foo as Foo
import qualified TestCases.Operations.TestCases.InlineObjectWithPropertiesAndAdditionalJsonResponse.Response200BodyItem as Response200BodyItem

data Response200Body = Response200Body
  { bar :: Maybe Bar.Bar
  , foo :: Foo.Foo
  , additionalProperties :: (Map.Map T.Text Response200BodyItem.Response200BodyItem)
  }
  deriving (Eq, Show)

response200BodySchema :: FC.Fleece t => FC.Schema t Response200Body
response200BodySchema =
  FC.object $
    FC.constructor Response200Body
      #+ FC.optional "bar" bar Bar.barSchema
      #+ FC.required "foo" foo Foo.fooSchema
      #* FC.additionalFields additionalProperties Response200BodyItem.response200BodyItemSchema