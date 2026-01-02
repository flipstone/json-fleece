{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ComicCollectionBase.CoverYear
  ( CoverYear(..)
  , coverYearSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype CoverYear = CoverYear Integer
  deriving (Show, Eq)

coverYearSchema :: FC.Fleece t => FC.Schema t CoverYear
coverYearSchema =
  FC.coerceSchema FC.integer