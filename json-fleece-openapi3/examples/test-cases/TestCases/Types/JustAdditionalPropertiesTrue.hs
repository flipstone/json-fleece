{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.JustAdditionalPropertiesTrue
  ( JustAdditionalPropertiesTrue(..)
  , justAdditionalPropertiesTrueSchema
  ) where

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype JustAdditionalPropertiesTrue = JustAdditionalPropertiesTrue (Map.Map T.Text FC.AnyJSON)
  deriving (Show, Eq)

justAdditionalPropertiesTrueSchema :: FC.Fleece schema => schema JustAdditionalPropertiesTrue
justAdditionalPropertiesTrueSchema =
  FC.coerceSchema (FC.jsonMap FC.anyJSON)
