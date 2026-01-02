{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TitleBase.FleetRank
  ( FleetRank(..)
  , fleetRankSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype FleetRank = FleetRank Bool
  deriving (Show, Eq)

fleetRankSchema :: FC.Fleece t => FC.Schema t FleetRank
fleetRankSchema =
  FC.coerceSchema FC.boolean