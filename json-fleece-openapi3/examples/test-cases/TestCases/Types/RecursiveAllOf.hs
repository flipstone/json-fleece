{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.RecursiveAllOf
  ( RecursiveAllOf(..)
  , recursiveAllOfSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Types.RecursiveAllOf.Age as Age
import qualified TestCases.Types.RecursiveAllOf.Description as Description
import qualified TestCases.Types.RecursiveAllOf.Id as Id
import qualified TestCases.Types.RecursiveAllOf.Name as Name
import qualified TestCases.Types.RecursiveAllOf.Type as Type

data RecursiveAllOf = RecursiveAllOf
  { age :: Maybe Age.Age
  , description :: Maybe Description.Description
  , id :: Id.Id
  , name :: Maybe Name.Name
  , type_ :: Type.Type
  }
  deriving (Eq, Show)

recursiveAllOfSchema :: FC.Fleece schema => schema RecursiveAllOf
recursiveAllOfSchema =
  FC.object $
    FC.constructor RecursiveAllOf
      #+ FC.optional "age" age Age.ageSchema
      #+ FC.optional "description" description Description.descriptionSchema
      #+ FC.required "id" id Id.idSchema
      #+ FC.optional "name" name Name.nameSchema
      #+ FC.required "type" type_ Type.typeSchema