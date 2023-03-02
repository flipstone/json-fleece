{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TechnologyFull.FictionalTechnology
  ( FictionalTechnology(..)
  , fictionalTechnologySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype FictionalTechnology = FictionalTechnology Bool
  deriving (Show, Eq)

fictionalTechnologySchema :: FC.Fleece schema => schema FictionalTechnology
fictionalTechnologySchema =
  FC.coerceSchema FC.boolean