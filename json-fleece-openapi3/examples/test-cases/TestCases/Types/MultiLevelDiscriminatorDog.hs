{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.MultiLevelDiscriminatorDog
  ( MultiLevelDiscriminatorDog(..)
  , multiLevelDiscriminatorDogSchema
  , multiLevelDiscriminatorDogObjSchema
  ) where

import Fleece.Core (Object)
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

data MultiLevelDiscriminatorDog = MultiLevelDiscriminatorDog
  deriving (Eq, Show)

multiLevelDiscriminatorDogSchema :: FC.Fleece t => FC.Schema t MultiLevelDiscriminatorDog
multiLevelDiscriminatorDogSchema =
  FC.object multiLevelDiscriminatorDogObjSchema

multiLevelDiscriminatorDogObjSchema :: FC.Fleece schema => Object schema MultiLevelDiscriminatorDog MultiLevelDiscriminatorDog
multiLevelDiscriminatorDogObjSchema =
  FC.constructor MultiLevelDiscriminatorDog