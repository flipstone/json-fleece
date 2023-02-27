{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.LocationFull.Landmark
  ( Landmark(..)
  , landmarkSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Landmark = Landmark Bool
  deriving (Show, Eq)

landmarkSchema :: FC.Fleece schema => schema Landmark
landmarkSchema =
  FC.coerceSchema FC.boolean