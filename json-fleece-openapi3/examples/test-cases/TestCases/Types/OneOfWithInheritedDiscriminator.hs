{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module TestCases.Types.OneOfWithInheritedDiscriminator
  ( OneOfWithInheritedDiscriminator(..)
  , oneOfWithInheritedDiscriminatorSchema
  ) where

import Fleece.Core ((#@))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import Shrubbery (type (@=))
import qualified Shrubbery as Shrubbery
import qualified TestCases.Types.InheritedDiscriminatorChildA as InheritedDiscriminatorChildA
import qualified TestCases.Types.InheritedDiscriminatorChildB as InheritedDiscriminatorChildB
import qualified TestCases.Types.InheritedDiscriminatorChildC as InheritedDiscriminatorChildC

newtype OneOfWithInheritedDiscriminator = OneOfWithInheritedDiscriminator (Shrubbery.TaggedUnion
  '[ "childA" @= InheritedDiscriminatorChildA.InheritedDiscriminatorChildA
   , "childB" @= InheritedDiscriminatorChildB.InheritedDiscriminatorChildB
   , "childC" @= InheritedDiscriminatorChildC.InheritedDiscriminatorChildC
   ])
  deriving (Show, Eq)

oneOfWithInheritedDiscriminatorSchema :: FC.Fleece t => FC.Schema t OneOfWithInheritedDiscriminator
oneOfWithInheritedDiscriminatorSchema =
  FC.coerceSchema $
    FC.taggedUnionNamed (FC.qualifiedName "TestCases.Types.OneOfWithInheritedDiscriminator" "OneOfWithInheritedDiscriminator") "type" $
      FC.taggedUnionMember @"childA" InheritedDiscriminatorChildA.inheritedDiscriminatorChildAObjSchema
        #@ FC.taggedUnionMember @"childB" InheritedDiscriminatorChildB.inheritedDiscriminatorChildBObjSchema
        #@ FC.taggedUnionMember @"childC" InheritedDiscriminatorChildC.inheritedDiscriminatorChildCObjSchema