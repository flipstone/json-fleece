{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.AllOfNoObject
  ( AllOfNoObject(..)
  , allOfNoObjectSchema
  ) where

import qualified Data.Time as Time
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype AllOfNoObject = AllOfNoObject Time.Day
  deriving (Show, Eq)

allOfNoObjectSchema :: FC.Fleece schema => schema AllOfNoObject
allOfNoObjectSchema =
  FC.coerceSchema FC.day