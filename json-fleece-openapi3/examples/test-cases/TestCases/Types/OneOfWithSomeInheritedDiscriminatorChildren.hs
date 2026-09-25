{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}

module TestCases.Types.OneOfWithSomeInheritedDiscriminatorChildren
  ( OneOfWithSomeInheritedDiscriminatorChildren(..)
  , oneOfWithSomeInheritedDiscriminatorChildrenSchema
  ) where

import Fleece.Core ((#|))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified Shrubbery as Shrubbery
import qualified TestCases.Types.InheritedDiscriminatorChildA as InheritedDiscriminatorChildA
import qualified TestCases.Types.InheritedDiscriminatorChildB as InheritedDiscriminatorChildB

newtype OneOfWithSomeInheritedDiscriminatorChildren = OneOfWithSomeInheritedDiscriminatorChildren (Shrubbery.Union
  '[ InheritedDiscriminatorChildA.InheritedDiscriminatorChildA
   , InheritedDiscriminatorChildB.InheritedDiscriminatorChildB
   ])
  deriving (Show, Eq)

oneOfWithSomeInheritedDiscriminatorChildrenSchema :: FC.Fleece t => FC.Schema t OneOfWithSomeInheritedDiscriminatorChildren
oneOfWithSomeInheritedDiscriminatorChildrenSchema =
  FC.coerceSchema $
    FC.unionNamed (FC.qualifiedName "TestCases.Types.OneOfWithSomeInheritedDiscriminatorChildren" "OneOfWithSomeInheritedDiscriminatorChildren") $
      FC.unionMember InheritedDiscriminatorChildA.inheritedDiscriminatorChildASchema
        #| FC.unionMember InheritedDiscriminatorChildB.inheritedDiscriminatorChildBSchema