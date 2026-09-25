{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.PartiallyInheritedDiscriminatorNonChild
  ( PartiallyInheritedDiscriminatorNonChild(..)
  , partiallyInheritedDiscriminatorNonChildSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Types.PartiallyInheritedDiscriminatorNonChild.NonChildName as NonChildName
import qualified TestCases.Types.PartiallyInheritedDiscriminatorNonChild.Type as Type

data PartiallyInheritedDiscriminatorNonChild = PartiallyInheritedDiscriminatorNonChild
  { nonChildName :: Maybe NonChildName.NonChildName
  , type_ :: Type.Type
  }
  deriving (Eq, Show)

partiallyInheritedDiscriminatorNonChildSchema :: FC.Fleece t => FC.Schema t PartiallyInheritedDiscriminatorNonChild
partiallyInheritedDiscriminatorNonChildSchema =
  FC.object $
    FC.constructor PartiallyInheritedDiscriminatorNonChild
      #+ FC.optional "nonChildName" nonChildName NonChildName.nonChildNameSchema
      #+ FC.required "type" type_ Type.typeSchema