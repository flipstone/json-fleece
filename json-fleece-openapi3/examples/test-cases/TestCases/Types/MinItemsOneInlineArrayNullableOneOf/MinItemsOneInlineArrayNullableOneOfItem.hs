{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}

module TestCases.Types.MinItemsOneInlineArrayNullableOneOf.MinItemsOneInlineArrayNullableOneOfItem
  ( MinItemsOneInlineArrayNullableOneOfItem(..)
  , minItemsOneInlineArrayNullableOneOfItemSchema
  ) where

import qualified Data.List.NonEmpty as NEL
import qualified Fleece.Core as FC
import Prelude (($), Bool, Either, Eq, Show)
import qualified Shrubbery as Shrubbery

newtype MinItemsOneInlineArrayNullableOneOfItem = MinItemsOneInlineArrayNullableOneOfItem (Shrubbery.Union
  '[ Either FC.Null (NEL.NonEmpty Bool)
   ])
  deriving (Show, Eq)

minItemsOneInlineArrayNullableOneOfItemSchema :: FC.Fleece schema => schema MinItemsOneInlineArrayNullableOneOfItem
minItemsOneInlineArrayNullableOneOfItemSchema =
  FC.coerceSchema $
    FC.unionNamed (FC.qualifiedName "TestCases.Types.MinItemsOneInlineArrayNullableOneOf.MinItemsOneInlineArrayNullableOneOfItem" "MinItemsOneInlineArrayNullableOneOfItem") $
      FC.unionMember (FC.nullable (FC.nonEmpty FC.boolean))