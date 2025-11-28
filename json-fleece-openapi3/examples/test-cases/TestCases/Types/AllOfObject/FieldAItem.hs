{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}

module TestCases.Types.AllOfObject.FieldAItem
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

fieldAItemSchema :: FC.Fleece schema => schema FieldAItem
fieldAItemSchema =
  FC.coerceSchema $
    FC.unionNamed (FC.qualifiedName "TestCases.Types.AllOfObject.FieldAItem" "FieldAItem") $
      FC.unionMember FC.text
        #| FC.unionMember FC.integer