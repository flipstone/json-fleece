{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.ZonedTimeDiscriminatorMember
  ( ZonedTimeDiscriminatorMember(..)
  , zonedTimeDiscriminatorMemberSchema
  , zonedTimeDiscriminatorMemberObjSchema
  ) where

import Fleece.Core ((#+), Object)
import qualified Fleece.Core as FC
import Prelude (Maybe, Show)
import qualified TestCases.Types.ZonedTimeType as ZonedTimeType

newtype ZonedTimeDiscriminatorMember = ZonedTimeDiscriminatorMember
  { time :: Maybe ZonedTimeType.ZonedTimeType
  }
  deriving (Show)

zonedTimeDiscriminatorMemberSchema :: FC.Fleece t => FC.Schema t ZonedTimeDiscriminatorMember
zonedTimeDiscriminatorMemberSchema =
  FC.object zonedTimeDiscriminatorMemberObjSchema

zonedTimeDiscriminatorMemberObjSchema :: FC.Fleece schema => Object schema ZonedTimeDiscriminatorMember ZonedTimeDiscriminatorMember
zonedTimeDiscriminatorMemberObjSchema =
  FC.constructor ZonedTimeDiscriminatorMember
    #+ FC.optional "time" time ZonedTimeType.zonedTimeTypeSchema