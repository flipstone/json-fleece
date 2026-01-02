{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.PerformerBase.VoyPerformer
  ( VoyPerformer(..)
  , voyPerformerSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype VoyPerformer = VoyPerformer Bool
  deriving (Show, Eq)

voyPerformerSchema :: FC.Fleece t => FC.Schema t VoyPerformer
voyPerformerSchema =
  FC.coerceSchema FC.boolean