{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}

module TestCases.Types.OneOfWithInlineRefArrayItems
  ( OneOfWithInlineRefArrayItems(..)
  , oneOfWithInlineRefArrayItemsSchema
  ) where

import qualified Data.Map as Map
import qualified Data.Text as T
import Fleece.Core ((#|))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified Shrubbery as Shrubbery
import qualified TestCases.Types.Foo as Foo

newtype OneOfWithInlineRefArrayItems = OneOfWithInlineRefArrayItems (Shrubbery.Union
  '[ (Map.Map T.Text [Foo.Foo])
   , T.Text
   ])
  deriving (Show, Eq)

oneOfWithInlineRefArrayItemsSchema :: FC.Fleece t => FC.Schema t OneOfWithInlineRefArrayItems
oneOfWithInlineRefArrayItemsSchema =
  FC.coerceSchema $
    FC.unionNamed (FC.qualifiedName "TestCases.Types.OneOfWithInlineRefArrayItems" "OneOfWithInlineRefArrayItems") $
      FC.unionMember (FC.map (FC.list Foo.fooSchema))
        #| FC.unionMember FC.text