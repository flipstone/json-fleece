{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ComicsBase.CoverYear
  ( CoverYear(..)
  , coverYearSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype CoverYear = CoverYear Integer
  deriving (Show, Eq)

coverYearSchema :: FC.Fleece schema => schema CoverYear
coverYearSchema =
  FC.coerceSchema FC.integer