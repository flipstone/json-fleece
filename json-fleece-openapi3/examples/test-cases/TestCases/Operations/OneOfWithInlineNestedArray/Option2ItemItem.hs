{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.OneOfWithInlineNestedArray.Option2ItemItem
  ( Option2ItemItem(..)
  , option2ItemItemSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Operations.OneOfWithInlineNestedArray.Option2ItemItem.Id as Id
import qualified TestCases.Operations.OneOfWithInlineNestedArray.Option2ItemItem.Name as Name

data Option2ItemItem = Option2ItemItem
  { id :: Maybe Id.Id
  , name :: Maybe Name.Name
  }
  deriving (Eq, Show)

option2ItemItemSchema :: FC.Fleece schema => schema Option2ItemItem
option2ItemItemSchema =
  FC.object $
    FC.constructor Option2ItemItem
      #+ FC.optional "id" id Id.idSchema
      #+ FC.optional "name" name Name.nameSchema