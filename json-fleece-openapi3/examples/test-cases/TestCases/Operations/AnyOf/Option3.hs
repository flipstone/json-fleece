{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.AnyOf.Option3
  ( Option3(..)
  , option3Schema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Operations.AnyOf.Option3.Description as Description
import qualified TestCases.Operations.AnyOf.Option3.Id as Id
import qualified TestCases.Operations.AnyOf.Option3.Name as Name

data Option3 = Option3
  { description :: Maybe Description.Description
  , id :: Id.Id
  , name :: Name.Name
  }
  deriving (Eq, Show)

option3Schema :: FC.Fleece schema => schema Option3
option3Schema =
  FC.object $
    FC.constructor Option3
      #+ FC.optional "description" description Description.descriptionSchema
      #+ FC.required "id" id Id.idSchema
      #+ FC.required "name" name Name.nameSchema