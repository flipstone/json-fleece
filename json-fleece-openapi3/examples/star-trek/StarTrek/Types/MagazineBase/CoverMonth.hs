{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MagazineBase.CoverMonth
  ( CoverMonth(..)
  , coverMonthSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype CoverMonth = CoverMonth Integer
  deriving (Show, Eq)

coverMonthSchema :: FC.Fleece t => FC.Schema t CoverMonth
coverMonthSchema =
  FC.coerceSchema FC.integer