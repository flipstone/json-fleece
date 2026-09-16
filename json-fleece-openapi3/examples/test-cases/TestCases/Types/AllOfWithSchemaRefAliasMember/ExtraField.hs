{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.AllOfWithSchemaRefAliasMember.ExtraField
  ( ExtraField(..)
  , extraFieldSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype ExtraField = ExtraField T.Text
  deriving (Show, Eq)

extraFieldSchema :: FC.Fleece t => FC.Schema t ExtraField
extraFieldSchema =
  FC.coerceSchema FC.text