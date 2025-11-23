{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.AllOf
  ( AllOf(..)
  , allOfSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Types.AllOf.Description as Description
import qualified TestCases.Types.AllOf.Id as Id
import qualified TestCases.Types.AllOf.Name as Name
import qualified TestCases.Types.AllOf.Type as Type

data AllOf = AllOf
  { description :: Maybe Description.Description
  , id :: Id.Id
  , name :: Maybe Name.Name
  , type_ :: Type.Type
  }
  deriving (Eq, Show)

allOfSchema :: FC.Fleece schema => schema AllOf
allOfSchema =
  FC.object $
    FC.constructor AllOf
      #+ FC.optional "description" description Description.descriptionSchema
      #+ FC.required "id" id Id.idSchema
      #+ FC.optional "name" name Name.nameSchema
      #+ FC.required "type" type_ Type.typeSchema