{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoGameBase.SystemRequirements
  ( SystemRequirements(..)
  , systemRequirementsSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype SystemRequirements = SystemRequirements Text
  deriving (Show, Eq)

systemRequirementsSchema :: FC.Fleece schema => schema SystemRequirements
systemRequirementsSchema =
  FC.coerceSchema FC.text