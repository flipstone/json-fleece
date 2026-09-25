{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.InheritedDiscriminatorParent
  ( InheritedDiscriminatorParent(..)
  , inheritedDiscriminatorParentSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified TestCases.Types.InheritedDiscriminatorParent.Type as Type

newtype InheritedDiscriminatorParent = InheritedDiscriminatorParent
  { type_ :: Type.Type
  }
  deriving (Eq, Show)

inheritedDiscriminatorParentSchema :: FC.Fleece t => FC.Schema t InheritedDiscriminatorParent
inheritedDiscriminatorParentSchema =
  FC.object $
    FC.constructor InheritedDiscriminatorParent
      #+ FC.required "type" type_ Type.typeSchema