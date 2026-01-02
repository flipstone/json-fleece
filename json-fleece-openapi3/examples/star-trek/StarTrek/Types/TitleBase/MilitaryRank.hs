{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TitleBase.MilitaryRank
  ( MilitaryRank(..)
  , militaryRankSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype MilitaryRank = MilitaryRank Bool
  deriving (Show, Eq)

militaryRankSchema :: FC.Fleece t => FC.Schema t MilitaryRank
militaryRankSchema =
  FC.coerceSchema FC.boolean