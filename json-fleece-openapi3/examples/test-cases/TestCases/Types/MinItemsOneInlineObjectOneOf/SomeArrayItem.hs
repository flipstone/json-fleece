{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.MinItemsOneInlineObjectOneOf.SomeArrayItem
  ( SomeArrayItem(..)
  , someArrayItemSchema
  ) where

import qualified Data.List.NonEmpty as NEL
import qualified Fleece.Core as FC
import Prelude (($), Bool, Eq, Show)
import qualified Shrubbery as Shrubbery

newtype SomeArrayItem = SomeArrayItem (Shrubbery.Union
  '[ (NEL.NonEmpty Bool)
   ])
  deriving (Show, Eq)

someArrayItemSchema :: FC.Fleece schema => schema SomeArrayItem
someArrayItemSchema =
  FC.coerceSchema $
    FC.unionNamed (FC.qualifiedName "TestCases.Types.MinItemsOneInlineObjectOneOf.SomeArrayItem" "SomeArrayItem") $
      FC.unionMember (FC.nonEmpty FC.boolean)