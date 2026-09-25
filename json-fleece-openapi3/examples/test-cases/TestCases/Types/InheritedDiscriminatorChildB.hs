{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.InheritedDiscriminatorChildB
  ( InheritedDiscriminatorChildB(..)
  , inheritedDiscriminatorChildBSchema
  , inheritedDiscriminatorChildBObjSchema
  ) where

import Fleece.Core ((#+), Object)
import qualified Fleece.Core as FC
import Prelude (Eq, Maybe, Show)
import qualified TestCases.Types.InheritedDiscriminatorChildB.ChildBName as ChildBName

newtype InheritedDiscriminatorChildB = InheritedDiscriminatorChildB
  { childBName :: Maybe ChildBName.ChildBName
  }
  deriving (Eq, Show)

inheritedDiscriminatorChildBSchema :: FC.Fleece t => FC.Schema t InheritedDiscriminatorChildB
inheritedDiscriminatorChildBSchema =
  FC.object inheritedDiscriminatorChildBObjSchema

inheritedDiscriminatorChildBObjSchema :: FC.Fleece schema => Object schema InheritedDiscriminatorChildB InheritedDiscriminatorChildB
inheritedDiscriminatorChildBObjSchema =
  FC.constructor InheritedDiscriminatorChildB
    #+ FC.optional "childBName" childBName ChildBName.childBNameSchema