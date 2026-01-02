{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.PerformerFull.TngPerformer
  ( TngPerformer(..)
  , tngPerformerSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype TngPerformer = TngPerformer Bool
  deriving (Show, Eq)

tngPerformerSchema :: FC.Fleece t => FC.Schema t TngPerformer
tngPerformerSchema =
  FC.coerceSchema FC.boolean