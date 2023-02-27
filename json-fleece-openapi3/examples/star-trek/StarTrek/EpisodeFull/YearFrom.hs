{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.EpisodeFull.YearFrom
  ( YearFrom(..)
  , yearFromSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype YearFrom = YearFrom Integer
  deriving (Show, Eq)

yearFromSchema :: FC.Fleece schema => schema YearFrom
yearFromSchema =
  FC.coerceSchema FC.integer