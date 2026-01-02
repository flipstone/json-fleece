{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.LocationBase.Landmark
  ( Landmark(..)
  , landmarkSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Landmark = Landmark Bool
  deriving (Show, Eq)

landmarkSchema :: FC.Fleece t => FC.Schema t Landmark
landmarkSchema =
  FC.coerceSchema FC.boolean