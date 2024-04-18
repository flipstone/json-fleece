{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}

module TestCases.Types.MinItemsOneInlineArrayOneOf.MinItemsOneInlineArrayOneOfItem
  ( MinItemsOneInlineArrayOneOfItem(..)
  , minItemsOneInlineArrayOneOfItemSchema
  ) where

import qualified Data.List.NonEmpty as NEL
import qualified Fleece.Core as FC
import Prelude (($), Bool, Eq, Show)
import qualified Shrubbery as Shrubbery

newtype MinItemsOneInlineArrayOneOfItem = MinItemsOneInlineArrayOneOfItem (Shrubbery.Union
  '[ (NEL.NonEmpty Bool)
   ])
  deriving (Show, Eq)

minItemsOneInlineArrayOneOfItemSchema :: FC.Fleece schema => schema MinItemsOneInlineArrayOneOfItem
minItemsOneInlineArrayOneOfItemSchema =
  FC.coerceSchema $
    FC.unionNamed (FC.qualifiedName "TestCases.Types.MinItemsOneInlineArrayOneOf.MinItemsOneInlineArrayOneOfItem" "MinItemsOneInlineArrayOneOfItem") $
      FC.unionMember (FC.nonEmpty FC.boolean)