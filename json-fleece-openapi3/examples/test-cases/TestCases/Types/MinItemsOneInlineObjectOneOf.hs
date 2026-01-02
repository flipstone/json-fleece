{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.MinItemsOneInlineObjectOneOf
  ( MinItemsOneInlineObjectOneOf(..)
  , minItemsOneInlineObjectOneOfSchema
  ) where

import qualified Data.List.NonEmpty as NEL
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Types.MinItemsOneInlineObjectOneOf.SomeArrayItem as SomeArrayItem

data MinItemsOneInlineObjectOneOf = MinItemsOneInlineObjectOneOf
  { someArray :: Maybe (NEL.NonEmpty SomeArrayItem.SomeArrayItem)
  }
  deriving (Eq, Show)

minItemsOneInlineObjectOneOfSchema :: FC.Fleece t => FC.Schema t MinItemsOneInlineObjectOneOf
minItemsOneInlineObjectOneOfSchema =
  FC.object $
    FC.constructor MinItemsOneInlineObjectOneOf
      #+ FC.optional "someArray" someArray (FC.nonEmpty SomeArrayItem.someArrayItemSchema)