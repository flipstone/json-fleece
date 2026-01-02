{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ComicCollectionBase.YearFrom
  ( YearFrom(..)
  , yearFromSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype YearFrom = YearFrom Integer
  deriving (Show, Eq)

yearFromSchema :: FC.Fleece t => FC.Schema t YearFrom
yearFromSchema =
  FC.coerceSchema FC.integer