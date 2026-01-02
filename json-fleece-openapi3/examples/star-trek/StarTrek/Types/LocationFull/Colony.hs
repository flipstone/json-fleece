{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.LocationFull.Colony
  ( Colony(..)
  , colonySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Colony = Colony Bool
  deriving (Show, Eq)

colonySchema :: FC.Fleece t => FC.Schema t Colony
colonySchema =
  FC.coerceSchema FC.boolean