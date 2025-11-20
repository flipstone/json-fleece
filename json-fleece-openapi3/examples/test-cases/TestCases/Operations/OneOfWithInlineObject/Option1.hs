{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.OneOfWithInlineObject.Option1
  ( Option1(..)
  , option1Schema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Operations.OneOfWithInlineObject.Option1.Id as Id
import qualified TestCases.Operations.OneOfWithInlineObject.Option1.Type as Type

data Option1 = Option1
  { id :: Maybe Id.Id
  , type_ :: Maybe Type.Type
  }
  deriving (Eq, Show)

option1Schema :: FC.Fleece schema => schema Option1
option1Schema =
  FC.object $
    FC.constructor Option1
      #+ FC.optional "id" id Id.idSchema
      #+ FC.optional "type" type_ Type.typeSchema