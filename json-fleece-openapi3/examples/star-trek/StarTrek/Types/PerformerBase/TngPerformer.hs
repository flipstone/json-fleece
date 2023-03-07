{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.PerformerBase.TngPerformer
  ( TngPerformer(..)
  , tngPerformerSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype TngPerformer = TngPerformer Bool
  deriving (Show, Eq)

tngPerformerSchema :: FC.Fleece schema => schema TngPerformer
tngPerformerSchema =
  FC.coerceSchema FC.boolean