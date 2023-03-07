{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.VideoReleaseBase.AmazonDigitalRelease
  ( AmazonDigitalRelease(..)
  , amazonDigitalReleaseSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype AmazonDigitalRelease = AmazonDigitalRelease Bool
  deriving (Show, Eq)

amazonDigitalReleaseSchema :: FC.Fleece schema => schema AmazonDigitalRelease
amazonDigitalReleaseSchema =
  FC.coerceSchema FC.boolean