{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}

module TestCases.Types.OneOfWithInlineNestedArray
  ( OneOfWithInlineNestedArray(..)
  , oneOfWithInlineNestedArraySchema
  ) where

import qualified Data.Text as T
import Fleece.Core ((#|))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified Shrubbery as Shrubbery
import qualified TestCases.Operations.OneOfWithInlineNestedArray.Option2ItemItem as Option2ItemItem

newtype OneOfWithInlineNestedArray = OneOfWithInlineNestedArray (Shrubbery.Union
  '[ [[T.Text]]
   , [[Option2ItemItem.Option2ItemItem]]
   ])
  deriving (Show, Eq)

oneOfWithInlineNestedArraySchema :: FC.Fleece t => FC.Schema t OneOfWithInlineNestedArray
oneOfWithInlineNestedArraySchema =
  FC.coerceSchema $
    FC.unionNamed (FC.qualifiedName "TestCases.Types.OneOfWithInlineNestedArray" "OneOfWithInlineNestedArray") $
      FC.unionMember (FC.list (FC.list FC.text))
        #| FC.unionMember (FC.list (FC.list Option2ItemItem.option2ItemItemSchema))