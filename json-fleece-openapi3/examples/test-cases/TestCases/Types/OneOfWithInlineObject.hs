{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}

module TestCases.Types.OneOfWithInlineObject
  ( OneOfWithInlineObject(..)
  , oneOfWithInlineObjectSchema
  ) where

import Fleece.Core ((#|))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified Shrubbery as Shrubbery
import qualified TestCases.Operations.OneOfWithInlineObject.Option1 as Option1
import qualified TestCases.Operations.OneOfWithInlineObject.Option3Item as Option3Item
import qualified TestCases.Operations.OneOfWithInlineObject.Option4 as Option4
import qualified TestCases.Types.Foo as Foo

newtype OneOfWithInlineObject = OneOfWithInlineObject (Shrubbery.Union
  '[ Option1.Option1
   , Foo.Foo
   , [Option3Item.Option3Item]
   , Option4.Option4
   ])
  deriving (Show, Eq)

oneOfWithInlineObjectSchema :: FC.Fleece t => FC.Schema t OneOfWithInlineObject
oneOfWithInlineObjectSchema =
  FC.coerceSchema $
    FC.unionNamed (FC.qualifiedName "TestCases.Types.OneOfWithInlineObject" "OneOfWithInlineObject") $
      FC.unionMember Option1.option1Schema
        #| FC.unionMember Foo.fooSchema
        #| FC.unionMember (FC.list Option3Item.option3ItemSchema)
        #| FC.unionMember Option4.option4Schema