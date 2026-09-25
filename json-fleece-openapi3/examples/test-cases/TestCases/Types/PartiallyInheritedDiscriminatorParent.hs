{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.PartiallyInheritedDiscriminatorParent
  ( PartiallyInheritedDiscriminatorParent(..)
  , partiallyInheritedDiscriminatorParentSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified TestCases.Types.PartiallyInheritedDiscriminatorParent.Type as Type

newtype PartiallyInheritedDiscriminatorParent = PartiallyInheritedDiscriminatorParent
  { type_ :: Type.Type
  }
  deriving (Eq, Show)

partiallyInheritedDiscriminatorParentSchema :: FC.Fleece t => FC.Schema t PartiallyInheritedDiscriminatorParent
partiallyInheritedDiscriminatorParentSchema =
  FC.object $
    FC.constructor PartiallyInheritedDiscriminatorParent
      #+ FC.required "type" type_ Type.typeSchema