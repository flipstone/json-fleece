{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}

module TestCases.Types.DateTimeFormats.ZonedTimeInUnionField
  ( ZonedTimeInUnionField(..)
  , zonedTimeInUnionFieldSchema
  ) where

import qualified Data.Text as T
import Fleece.Core ((#|))
import qualified Fleece.Core as FC
import Prelude (($), Show)
import qualified Shrubbery as Shrubbery
import qualified TestCases.Types.ZonedTimeType as ZonedTimeType

newtype ZonedTimeInUnionField = ZonedTimeInUnionField (Shrubbery.Union
  '[ ZonedTimeType.ZonedTimeType
   , T.Text
   ])
  deriving (Show)

zonedTimeInUnionFieldSchema :: FC.Fleece t => FC.Schema t ZonedTimeInUnionField
zonedTimeInUnionFieldSchema =
  FC.coerceSchema $
    FC.unionNamed (FC.qualifiedName "TestCases.Types.DateTimeFormats.ZonedTimeInUnionField" "ZonedTimeInUnionField") $
      FC.unionMember ZonedTimeType.zonedTimeTypeSchema
        #| FC.unionMember FC.text