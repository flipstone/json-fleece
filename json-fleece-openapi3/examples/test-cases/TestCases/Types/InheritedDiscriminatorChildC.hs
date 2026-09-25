{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.InheritedDiscriminatorChildC
  ( InheritedDiscriminatorChildC(..)
  , inheritedDiscriminatorChildCSchema
  , inheritedDiscriminatorChildCObjSchema
  ) where

import Fleece.Core ((#+), Object)
import qualified Fleece.Core as FC
import Prelude (Eq, Maybe, Show)
import qualified TestCases.Types.InheritedDiscriminatorChildC.ChildCName as ChildCName

newtype InheritedDiscriminatorChildC = InheritedDiscriminatorChildC
  { childCName :: Maybe ChildCName.ChildCName
  }
  deriving (Eq, Show)

inheritedDiscriminatorChildCSchema :: FC.Fleece t => FC.Schema t InheritedDiscriminatorChildC
inheritedDiscriminatorChildCSchema =
  FC.object inheritedDiscriminatorChildCObjSchema

inheritedDiscriminatorChildCObjSchema :: FC.Fleece schema => Object schema InheritedDiscriminatorChildC InheritedDiscriminatorChildC
inheritedDiscriminatorChildCObjSchema =
  FC.constructor InheritedDiscriminatorChildC
    #+ FC.optional "childCName" childCName ChildCName.childCNameSchema