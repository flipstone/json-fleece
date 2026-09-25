{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.MultiLevelDiscriminatorCat
  ( MultiLevelDiscriminatorCat(..)
  , multiLevelDiscriminatorCatSchema
  , multiLevelDiscriminatorCatObjSchema
  ) where

import Fleece.Core ((#+), Object)
import qualified Fleece.Core as FC
import Prelude (Eq, Maybe, Show)
import qualified TestCases.Types.MultiLevelDiscriminatorCat.Whiskers as Whiskers

newtype MultiLevelDiscriminatorCat = MultiLevelDiscriminatorCat
  { whiskers :: Maybe Whiskers.Whiskers
  }
  deriving (Eq, Show)

multiLevelDiscriminatorCatSchema :: FC.Fleece t => FC.Schema t MultiLevelDiscriminatorCat
multiLevelDiscriminatorCatSchema =
  FC.object multiLevelDiscriminatorCatObjSchema

multiLevelDiscriminatorCatObjSchema :: FC.Fleece schema => Object schema MultiLevelDiscriminatorCat MultiLevelDiscriminatorCat
multiLevelDiscriminatorCatObjSchema =
  FC.constructor MultiLevelDiscriminatorCat
    #+ FC.optional "whiskers" whiskers Whiskers.whiskersSchema