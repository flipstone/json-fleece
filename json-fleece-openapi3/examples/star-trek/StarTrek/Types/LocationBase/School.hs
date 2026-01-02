{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.LocationBase.School
  ( School(..)
  , schoolSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype School = School Bool
  deriving (Show, Eq)

schoolSchema :: FC.Fleece t => FC.Schema t School
schoolSchema =
  FC.coerceSchema FC.boolean