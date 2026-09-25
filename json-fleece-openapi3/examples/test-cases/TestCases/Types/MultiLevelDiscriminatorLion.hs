{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.MultiLevelDiscriminatorLion
  ( MultiLevelDiscriminatorLion(..)
  , multiLevelDiscriminatorLionSchema
  , multiLevelDiscriminatorLionObjSchema
  ) where

import Fleece.Core ((#+), Object)
import qualified Fleece.Core as FC
import Prelude (Eq, Maybe, Show)
import qualified TestCases.Types.MultiLevelDiscriminatorLion.Whiskers as Whiskers

newtype MultiLevelDiscriminatorLion = MultiLevelDiscriminatorLion
  { whiskers :: Maybe Whiskers.Whiskers
  }
  deriving (Eq, Show)

multiLevelDiscriminatorLionSchema :: FC.Fleece t => FC.Schema t MultiLevelDiscriminatorLion
multiLevelDiscriminatorLionSchema =
  FC.object multiLevelDiscriminatorLionObjSchema

multiLevelDiscriminatorLionObjSchema :: FC.Fleece schema => Object schema MultiLevelDiscriminatorLion MultiLevelDiscriminatorLion
multiLevelDiscriminatorLionObjSchema =
  FC.constructor MultiLevelDiscriminatorLion
    #+ FC.optional "whiskers" whiskers Whiskers.whiskersSchema