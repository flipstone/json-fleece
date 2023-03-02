{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoReleaseFull.YearFrom
  ( YearFrom(..)
  , yearFromSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype YearFrom = YearFrom Integer
  deriving (Show, Eq)

yearFromSchema :: FC.Fleece schema => schema YearFrom
yearFromSchema =
  FC.coerceSchema FC.integer