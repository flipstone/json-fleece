{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.MultiLevelDiscriminatorAnimal
  ( MultiLevelDiscriminatorAnimal(..)
  , multiLevelDiscriminatorAnimalSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified TestCases.Types.MultiLevelDiscriminatorAnimal.Type as Type

newtype MultiLevelDiscriminatorAnimal = MultiLevelDiscriminatorAnimal
  { type_ :: Type.Type
  }
  deriving (Eq, Show)

multiLevelDiscriminatorAnimalSchema :: FC.Fleece t => FC.Schema t MultiLevelDiscriminatorAnimal
multiLevelDiscriminatorAnimalSchema =
  FC.object $
    FC.constructor MultiLevelDiscriminatorAnimal
      #+ FC.required "type" type_ Type.typeSchema