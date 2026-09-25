{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.MultiLevelDiscriminatorFeline
  ( MultiLevelDiscriminatorFeline(..)
  , multiLevelDiscriminatorFelineSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Types.MultiLevelDiscriminatorFeline.Type as Type
import qualified TestCases.Types.MultiLevelDiscriminatorFeline.Whiskers as Whiskers

data MultiLevelDiscriminatorFeline = MultiLevelDiscriminatorFeline
  { type_ :: Type.Type
  , whiskers :: Maybe Whiskers.Whiskers
  }
  deriving (Eq, Show)

multiLevelDiscriminatorFelineSchema :: FC.Fleece t => FC.Schema t MultiLevelDiscriminatorFeline
multiLevelDiscriminatorFelineSchema =
  FC.object $
    FC.constructor MultiLevelDiscriminatorFeline
      #+ FC.required "type" type_ Type.typeSchema
      #+ FC.optional "whiskers" whiskers Whiskers.whiskersSchema