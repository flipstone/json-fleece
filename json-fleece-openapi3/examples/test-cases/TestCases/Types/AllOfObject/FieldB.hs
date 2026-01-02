{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.AllOfObject.FieldB
  ( FieldB(..)
  , fieldBSchema
  ) where

import qualified Data.Time as Time
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype FieldB = FieldB Time.UTCTime
  deriving (Show, Eq)

fieldBSchema :: FC.Fleece t => FC.Schema t FieldB
fieldBSchema =
  FC.coerceSchema FC.utcTime