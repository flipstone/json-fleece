{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MagazineBase.CoverYear
  ( CoverYear(..)
  , coverYearSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype CoverYear = CoverYear Integer
  deriving (Show, Eq)

coverYearSchema :: FC.Fleece schema => schema CoverYear
coverYearSchema =
  FC.coerceSchema FC.integer