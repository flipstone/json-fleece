{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.AnimalBase.EarthInsect
  ( EarthInsect(..)
  , earthInsectSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype EarthInsect = EarthInsect Bool
  deriving (Show, Eq)

earthInsectSchema :: FC.Fleece schema => schema EarthInsect
earthInsectSchema =
  FC.coerceSchema FC.boolean