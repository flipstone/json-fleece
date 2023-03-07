{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.AnimalFull.EarthInsect
  ( EarthInsect(..)
  , earthInsectSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype EarthInsect = EarthInsect Bool
  deriving (Show, Eq)

earthInsectSchema :: FC.Fleece schema => schema EarthInsect
earthInsectSchema =
  FC.coerceSchema FC.boolean