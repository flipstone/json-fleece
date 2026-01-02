{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module TestCases.Types.OneOfWithDiscriminator
  ( OneOfWithDiscriminator(..)
  , oneOfWithDiscriminatorSchema
  ) where

import Fleece.Core ((#@))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import Shrubbery (type (@=))
import qualified Shrubbery as Shrubbery
import qualified TestCases.Types.Bar as Bar
import qualified TestCases.Types.Baz as Baz
import qualified TestCases.Types.Foo as Foo

newtype OneOfWithDiscriminator = OneOfWithDiscriminator (Shrubbery.TaggedUnion
  '[ "bar" @= Bar.Bar
   , "baz" @= Baz.Baz
   , "foo" @= Foo.Foo
   ])
  deriving (Show, Eq)

oneOfWithDiscriminatorSchema :: FC.Fleece t => FC.Schema t OneOfWithDiscriminator
oneOfWithDiscriminatorSchema =
  FC.coerceSchema $
    FC.taggedUnionNamed (FC.qualifiedName "TestCases.Types.OneOfWithDiscriminator" "OneOfWithDiscriminator") "type" $
      FC.taggedUnionMember @"bar" Bar.barObjSchema
        #@ FC.taggedUnionMember @"baz" Baz.bazObjSchema
        #@ FC.taggedUnionMember @"foo" Foo.fooObjSchema