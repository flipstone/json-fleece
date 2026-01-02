{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.LocationBase.Shipyard
  ( Shipyard(..)
  , shipyardSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Shipyard = Shipyard Bool
  deriving (Show, Eq)

shipyardSchema :: FC.Fleece t => FC.Schema t Shipyard
shipyardSchema =
  FC.coerceSchema FC.boolean