{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.EpisodeBase.FeatureLength
  ( FeatureLength(..)
  , featureLengthSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype FeatureLength = FeatureLength Bool
  deriving (Show, Eq)

featureLengthSchema :: FC.Fleece t => FC.Schema t FeatureLength
featureLengthSchema =
  FC.coerceSchema FC.boolean