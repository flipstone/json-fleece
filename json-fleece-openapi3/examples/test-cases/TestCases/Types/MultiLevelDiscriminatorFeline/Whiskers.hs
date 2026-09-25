{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.MultiLevelDiscriminatorFeline.Whiskers
  ( Whiskers(..)
  , whiskersSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype Whiskers = Whiskers Integer
  deriving (Show, Eq)

whiskersSchema :: FC.Fleece t => FC.Schema t Whiskers
whiskersSchema =
  FC.coerceSchema FC.integer