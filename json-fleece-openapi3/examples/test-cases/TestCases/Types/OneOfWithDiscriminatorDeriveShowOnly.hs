{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module TestCases.Types.OneOfWithDiscriminatorDeriveShowOnly
  ( OneOfWithDiscriminatorDeriveShowOnly(..)
  , oneOfWithDiscriminatorDeriveShowOnlySchema
  ) where

import Fleece.Core ((#@))
import qualified Fleece.Core as FC
import Prelude (($), Show)
import Shrubbery (type (@=))
import qualified Shrubbery as Shrubbery
import qualified TestCases.Types.Foo as Foo
import qualified TestCases.Types.ZonedTimeDiscriminatorMember as ZonedTimeDiscriminatorMember

newtype OneOfWithDiscriminatorDeriveShowOnly = OneOfWithDiscriminatorDeriveShowOnly (Shrubbery.TaggedUnion
  '[ "foo" @= Foo.Foo
   , "zonedTime" @= ZonedTimeDiscriminatorMember.ZonedTimeDiscriminatorMember
   ])
  deriving (Show)

oneOfWithDiscriminatorDeriveShowOnlySchema :: FC.Fleece t => FC.Schema t OneOfWithDiscriminatorDeriveShowOnly
oneOfWithDiscriminatorDeriveShowOnlySchema =
  FC.coerceSchema $
    FC.taggedUnionNamed (FC.qualifiedName "TestCases.Types.OneOfWithDiscriminatorDeriveShowOnly" "OneOfWithDiscriminatorDeriveShowOnly") "type" $
      FC.taggedUnionMember @"foo" Foo.fooObjSchema
        #@ FC.taggedUnionMember @"zonedTime" ZonedTimeDiscriminatorMember.zonedTimeDiscriminatorMemberObjSchema