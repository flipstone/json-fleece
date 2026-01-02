{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}

module TestCases.Operations.InlineAllOf.Response200Body.FieldAItem
  ( FieldAItem(..)
  , fieldAItemSchema
  ) where

import qualified Data.Text as T
import Fleece.Core ((#|))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Integer, Show)
import qualified Shrubbery as Shrubbery

newtype FieldAItem = FieldAItem (Shrubbery.Union
  '[ T.Text
   , Integer
   ])
  deriving (Show, Eq)

fieldAItemSchema :: FC.Fleece t => FC.Schema t FieldAItem
fieldAItemSchema =
  FC.coerceSchema $
    FC.unionNamed (FC.qualifiedName "TestCases.Operations.InlineAllOf.Response200Body.FieldAItem" "FieldAItem") $
      FC.unionMember FC.text
        #| FC.unionMember FC.integer