{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.PartiallyInheritedDiscriminatorChild
  ( PartiallyInheritedDiscriminatorChild(..)
  , partiallyInheritedDiscriminatorChildSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Types.PartiallyInheritedDiscriminatorChild.ChildName as ChildName
import qualified TestCases.Types.PartiallyInheritedDiscriminatorChild.Type as Type

data PartiallyInheritedDiscriminatorChild = PartiallyInheritedDiscriminatorChild
  { childName :: Maybe ChildName.ChildName
  , type_ :: Type.Type
  }
  deriving (Eq, Show)

partiallyInheritedDiscriminatorChildSchema :: FC.Fleece t => FC.Schema t PartiallyInheritedDiscriminatorChild
partiallyInheritedDiscriminatorChildSchema =
  FC.object $
    FC.constructor PartiallyInheritedDiscriminatorChild
      #+ FC.optional "childName" childName ChildName.childNameSchema
      #+ FC.required "type" type_ Type.typeSchema