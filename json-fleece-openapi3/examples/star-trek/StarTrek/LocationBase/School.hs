{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.LocationBase.School
  ( School(..)
  , schoolSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype School = School Bool
  deriving (Show, Eq)

schoolSchema :: FC.Fleece schema => schema School
schoolSchema =
  FC.coerceSchema FC.boolean