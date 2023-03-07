{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.PerformerFull.TasPerformer
  ( TasPerformer(..)
  , tasPerformerSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype TasPerformer = TasPerformer Bool
  deriving (Show, Eq)

tasPerformerSchema :: FC.Fleece schema => schema TasPerformer
tasPerformerSchema =
  FC.coerceSchema FC.boolean