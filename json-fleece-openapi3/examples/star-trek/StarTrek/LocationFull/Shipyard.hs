{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.LocationFull.Shipyard
  ( Shipyard(..)
  , shipyardSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Shipyard = Shipyard Bool
  deriving (Show, Eq)

shipyardSchema :: FC.Fleece schema => schema Shipyard
shipyardSchema =
  FC.coerceSchema FC.boolean