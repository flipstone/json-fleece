{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.VideoGameBase.SystemRequirements
  ( SystemRequirements(..)
  , systemRequirementsSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype SystemRequirements = SystemRequirements T.Text
  deriving (Show, Eq)

systemRequirementsSchema :: FC.Fleece t => FC.Schema t SystemRequirements
systemRequirementsSchema =
  FC.coerceSchema FC.text