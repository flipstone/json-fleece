{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module TestCases.Types.OneOfWithDiscriminatorTypeObject
  ( OneOfWithDiscriminatorTypeObject(..)
  , oneOfWithDiscriminatorTypeObjectSchema
  ) where

import Fleece.Core ((#@))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import Shrubbery (type (@=))
import qualified Shrubbery as Shrubbery
import qualified TestCases.Types.Bar as Bar
import qualified TestCases.Types.Baz as Baz
import qualified TestCases.Types.Foo as Foo

newtype OneOfWithDiscriminatorTypeObject = OneOfWithDiscriminatorTypeObject (Shrubbery.TaggedUnion
  '[ "bar" @= Bar.Bar
   , "baz" @= Baz.Baz
   , "foo" @= Foo.Foo
   ])
  deriving (Show, Eq)

oneOfWithDiscriminatorTypeObjectSchema :: FC.Fleece schema => schema OneOfWithDiscriminatorTypeObject
oneOfWithDiscriminatorTypeObjectSchema =
  FC.coerceSchema $
    FC.taggedUnionNamed (FC.qualifiedName "TestCases.Types.OneOfWithDiscriminatorTypeObject" "OneOfWithDiscriminatorTypeObject") "type" $
      FC.taggedUnionMember @"bar" Bar.barObjSchema
        #@ FC.taggedUnionMember @"baz" Baz.bazObjSchema
        #@ FC.taggedUnionMember @"foo" Foo.fooObjSchema