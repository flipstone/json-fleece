{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.LocationBase.Colony
  ( Colony(..)
  , colonySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Colony = Colony Bool
  deriving (Show, Eq)

colonySchema :: FC.Fleece schema => schema Colony
colonySchema =
  FC.coerceSchema FC.boolean