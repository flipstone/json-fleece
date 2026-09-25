{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module TestCases.Types.OneOfWithMultiLevelInheritedDiscriminator
  ( OneOfWithMultiLevelInheritedDiscriminator(..)
  , oneOfWithMultiLevelInheritedDiscriminatorSchema
  ) where

import Fleece.Core ((#@))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import Shrubbery (type (@=))
import qualified Shrubbery as Shrubbery
import qualified TestCases.Types.MultiLevelDiscriminatorCat as MultiLevelDiscriminatorCat
import qualified TestCases.Types.MultiLevelDiscriminatorDog as MultiLevelDiscriminatorDog
import qualified TestCases.Types.MultiLevelDiscriminatorLion as MultiLevelDiscriminatorLion

newtype OneOfWithMultiLevelInheritedDiscriminator = OneOfWithMultiLevelInheritedDiscriminator (Shrubbery.TaggedUnion
  '[ "cat" @= MultiLevelDiscriminatorCat.MultiLevelDiscriminatorCat
   , "dog" @= MultiLevelDiscriminatorDog.MultiLevelDiscriminatorDog
   , "lion" @= MultiLevelDiscriminatorLion.MultiLevelDiscriminatorLion
   ])
  deriving (Show, Eq)

oneOfWithMultiLevelInheritedDiscriminatorSchema :: FC.Fleece t => FC.Schema t OneOfWithMultiLevelInheritedDiscriminator
oneOfWithMultiLevelInheritedDiscriminatorSchema =
  FC.coerceSchema $
    FC.taggedUnionNamed (FC.qualifiedName "TestCases.Types.OneOfWithMultiLevelInheritedDiscriminator" "OneOfWithMultiLevelInheritedDiscriminator") "type" $
      FC.taggedUnionMember @"cat" MultiLevelDiscriminatorCat.multiLevelDiscriminatorCatObjSchema
        #@ FC.taggedUnionMember @"dog" MultiLevelDiscriminatorDog.multiLevelDiscriminatorDogObjSchema
        #@ FC.taggedUnionMember @"lion" MultiLevelDiscriminatorLion.multiLevelDiscriminatorLionObjSchema