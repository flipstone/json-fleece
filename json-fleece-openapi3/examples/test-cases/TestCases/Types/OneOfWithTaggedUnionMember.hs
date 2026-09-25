{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}

module TestCases.Types.OneOfWithTaggedUnionMember
  ( OneOfWithTaggedUnionMember(..)
  , oneOfWithTaggedUnionMemberSchema
  ) where

import Fleece.Core ((#|))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified Shrubbery as Shrubbery
import qualified TestCases.Types.InheritedDiscriminatorChildA as InheritedDiscriminatorChildA
import qualified TestCases.Types.OneOfWithDiscriminator as OneOfWithDiscriminator

newtype OneOfWithTaggedUnionMember = OneOfWithTaggedUnionMember (Shrubbery.Union
  '[ OneOfWithDiscriminator.OneOfWithDiscriminator
   , InheritedDiscriminatorChildA.InheritedDiscriminatorChildA
   ])
  deriving (Show, Eq)

oneOfWithTaggedUnionMemberSchema :: FC.Fleece t => FC.Schema t OneOfWithTaggedUnionMember
oneOfWithTaggedUnionMemberSchema =
  FC.coerceSchema $
    FC.unionNamed (FC.qualifiedName "TestCases.Types.OneOfWithTaggedUnionMember" "OneOfWithTaggedUnionMember") $
      FC.unionMember OneOfWithDiscriminator.oneOfWithDiscriminatorSchema
        #| FC.unionMember InheritedDiscriminatorChildA.inheritedDiscriminatorChildASchema