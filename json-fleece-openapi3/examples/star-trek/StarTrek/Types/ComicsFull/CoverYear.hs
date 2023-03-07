{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ComicsFull.CoverYear
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