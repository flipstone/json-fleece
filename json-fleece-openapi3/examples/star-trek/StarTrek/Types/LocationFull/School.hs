{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.LocationFull.School
  ( School(..)
  , schoolSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype School = School Bool
  deriving (Show, Eq)

schoolSchema :: FC.Fleece schema => schema School
schoolSchema =
  FC.coerceSchema FC.boolean