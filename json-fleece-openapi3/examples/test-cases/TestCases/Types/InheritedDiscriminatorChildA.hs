{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.InheritedDiscriminatorChildA
  ( InheritedDiscriminatorChildA(..)
  , inheritedDiscriminatorChildASchema
  , inheritedDiscriminatorChildAObjSchema
  ) where

import Fleece.Core ((#+), Object)
import qualified Fleece.Core as FC
import Prelude (Eq, Maybe, Show)
import qualified TestCases.Types.InheritedDiscriminatorChildA.ChildAName as ChildAName

newtype InheritedDiscriminatorChildA = InheritedDiscriminatorChildA
  { childAName :: Maybe ChildAName.ChildAName
  }
  deriving (Eq, Show)

inheritedDiscriminatorChildASchema :: FC.Fleece t => FC.Schema t InheritedDiscriminatorChildA
inheritedDiscriminatorChildASchema =
  FC.object inheritedDiscriminatorChildAObjSchema

inheritedDiscriminatorChildAObjSchema :: FC.Fleece schema => Object schema InheritedDiscriminatorChildA InheritedDiscriminatorChildA
inheritedDiscriminatorChildAObjSchema =
  FC.constructor InheritedDiscriminatorChildA
    #+ FC.optional "childAName" childAName ChildAName.childANameSchema