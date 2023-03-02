{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TitleBase.MilitaryRank
  ( MilitaryRank(..)
  , militaryRankSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype MilitaryRank = MilitaryRank Bool
  deriving (Show, Eq)

militaryRankSchema :: FC.Fleece schema => schema MilitaryRank
militaryRankSchema =
  FC.coerceSchema FC.boolean