{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.OneOfWithSingleInheritedDiscriminatorChild
  ( OneOfWithSingleInheritedDiscriminatorChild
  , oneOfWithSingleInheritedDiscriminatorChildSchema
  ) where

import qualified Fleece.Core as FC
import qualified TestCases.Types.InheritedDiscriminatorChildA as InheritedDiscriminatorChildA

type OneOfWithSingleInheritedDiscriminatorChild = InheritedDiscriminatorChildA.InheritedDiscriminatorChildA

oneOfWithSingleInheritedDiscriminatorChildSchema :: FC.Fleece t => FC.Schema t OneOfWithSingleInheritedDiscriminatorChild
oneOfWithSingleInheritedDiscriminatorChildSchema =
  FC.coerceSchemaNamed
    (FC.qualifiedName "TestCases.Types.OneOfWithSingleInheritedDiscriminatorChild" "OneOfWithSingleInheritedDiscriminatorChild")
    InheritedDiscriminatorChildA.inheritedDiscriminatorChildASchema