{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}

module TestCases.Types.OneOfWithPartiallyInheritedDiscriminator
  ( OneOfWithPartiallyInheritedDiscriminator(..)
  , oneOfWithPartiallyInheritedDiscriminatorSchema
  ) where

import Fleece.Core ((#|))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified Shrubbery as Shrubbery
import qualified TestCases.Types.PartiallyInheritedDiscriminatorChild as PartiallyInheritedDiscriminatorChild
import qualified TestCases.Types.PartiallyInheritedDiscriminatorNonChild as PartiallyInheritedDiscriminatorNonChild

newtype OneOfWithPartiallyInheritedDiscriminator = OneOfWithPartiallyInheritedDiscriminator (Shrubbery.Union
  '[ PartiallyInheritedDiscriminatorChild.PartiallyInheritedDiscriminatorChild
   , PartiallyInheritedDiscriminatorNonChild.PartiallyInheritedDiscriminatorNonChild
   ])
  deriving (Show, Eq)

oneOfWithPartiallyInheritedDiscriminatorSchema :: FC.Fleece t => FC.Schema t OneOfWithPartiallyInheritedDiscriminator
oneOfWithPartiallyInheritedDiscriminatorSchema =
  FC.coerceSchema $
    FC.unionNamed (FC.qualifiedName "TestCases.Types.OneOfWithPartiallyInheritedDiscriminator" "OneOfWithPartiallyInheritedDiscriminator") $
      FC.unionMember PartiallyInheritedDiscriminatorChild.partiallyInheritedDiscriminatorChildSchema
        #| FC.unionMember PartiallyInheritedDiscriminatorNonChild.partiallyInheritedDiscriminatorNonChildSchema