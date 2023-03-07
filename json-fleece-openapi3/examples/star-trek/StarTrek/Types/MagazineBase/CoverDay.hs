{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MagazineBase.CoverDay
  ( CoverDay(..)
  , coverDaySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype CoverDay = CoverDay Integer
  deriving (Show, Eq)

coverDaySchema :: FC.Fleece schema => schema CoverDay
coverDaySchema =
  FC.coerceSchema FC.integer