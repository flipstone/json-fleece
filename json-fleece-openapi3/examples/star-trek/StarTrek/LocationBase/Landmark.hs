{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.LocationBase.Landmark
  ( Landmark(..)
  , landmarkSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Landmark = Landmark Bool
  deriving (Show, Eq)

landmarkSchema :: FC.Fleece schema => schema Landmark
landmarkSchema =
  FC.coerceSchema FC.boolean