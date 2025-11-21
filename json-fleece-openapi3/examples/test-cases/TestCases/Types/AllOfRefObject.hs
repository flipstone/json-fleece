{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.AllOfRefObject
  ( AllOfRefObject(..)
  , allOfRefObjectSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Types.AllOfRefObject.Description as Description
import qualified TestCases.Types.AllOfRefObject.Id as Id

data AllOfRefObject = AllOfRefObject
  { description :: Maybe Description.Description
  , id :: Id.Id
  }
  deriving (Eq, Show)

allOfRefObjectSchema :: FC.Fleece schema => schema AllOfRefObject
allOfRefObjectSchema =
  FC.object $
    FC.constructor AllOfRefObject
      #+ FC.optional "description" description Description.descriptionSchema
      #+ FC.required "id" id Id.idSchema