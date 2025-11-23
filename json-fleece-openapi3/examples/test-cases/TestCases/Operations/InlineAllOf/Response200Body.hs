{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.InlineAllOf.Response200Body
  ( Response200Body(..)
  , response200BodySchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Types.InlineAllOf.Response200Body.Description as Description
import qualified TestCases.Types.InlineAllOf.Response200Body.Id as Id
import qualified TestCases.Types.InlineAllOf.Response200Body.Name as Name
import qualified TestCases.Types.InlineAllOf.Response200Body.Type as Type

data Response200Body = Response200Body
  { description :: Maybe Description.Description
  , id :: Id.Id
  , name :: Maybe Name.Name
  , type_ :: Type.Type
  }
  deriving (Eq, Show)

response200BodySchema :: FC.Fleece schema => schema Response200Body
response200BodySchema =
  FC.object $
    FC.constructor Response200Body
      #+ FC.optional "description" description Description.descriptionSchema
      #+ FC.required "id" id Id.idSchema
      #+ FC.optional "name" name Name.nameSchema
      #+ FC.required "type" type_ Type.typeSchema