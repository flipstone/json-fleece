{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.PerformerBase.EntPerformer
  ( EntPerformer(..)
  , entPerformerSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype EntPerformer = EntPerformer Bool
  deriving (Show, Eq)

entPerformerSchema :: FC.Fleece t => FC.Schema t EntPerformer
entPerformerSchema =
  FC.coerceSchema FC.boolean