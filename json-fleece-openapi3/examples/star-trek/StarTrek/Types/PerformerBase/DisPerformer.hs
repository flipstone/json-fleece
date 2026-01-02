{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.PerformerBase.DisPerformer
  ( DisPerformer(..)
  , disPerformerSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype DisPerformer = DisPerformer Bool
  deriving (Show, Eq)

disPerformerSchema :: FC.Fleece t => FC.Schema t DisPerformer
disPerformerSchema =
  FC.coerceSchema FC.boolean