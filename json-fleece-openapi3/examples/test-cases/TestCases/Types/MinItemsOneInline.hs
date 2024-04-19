{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.MinItemsOneInline
  ( MinItemsOneInline(..)
  , minItemsOneInlineSchema
  ) where

import qualified Data.List.NonEmpty as NEL
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Types.MinItemsOneInline.SomeArrayItem as SomeArrayItem

data MinItemsOneInline = MinItemsOneInline
  { someArray :: Maybe (NEL.NonEmpty SomeArrayItem.SomeArrayItem)
  }
  deriving (Eq, Show)

minItemsOneInlineSchema :: FC.Fleece schema => schema MinItemsOneInline
minItemsOneInlineSchema =
  FC.object $
    FC.constructor MinItemsOneInline
      #+ FC.optional "someArray" someArray (FC.nonEmpty SomeArrayItem.someArrayItemSchema)