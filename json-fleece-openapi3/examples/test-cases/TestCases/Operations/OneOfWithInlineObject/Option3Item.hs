{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.OneOfWithInlineObject.Option3Item
  ( Option3Item(..)
  , option3ItemSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Operations.OneOfWithInlineObject.Option3Item.Description as Description
import qualified TestCases.Operations.OneOfWithInlineObject.Option3Item.Type as Type

data Option3Item = Option3Item
  { description :: Maybe Description.Description
  , type_ :: Maybe Type.Type
  }
  deriving (Eq, Show)

option3ItemSchema :: FC.Fleece t => FC.Schema t Option3Item
option3ItemSchema =
  FC.object $
    FC.constructor Option3Item
      #+ FC.optional "description" description Description.descriptionSchema
      #+ FC.optional "type" type_ Type.typeSchema