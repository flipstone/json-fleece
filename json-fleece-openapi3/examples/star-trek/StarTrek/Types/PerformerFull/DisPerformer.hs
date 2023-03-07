{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.PerformerFull.DisPerformer
  ( DisPerformer(..)
  , disPerformerSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype DisPerformer = DisPerformer Bool
  deriving (Show, Eq)

disPerformerSchema :: FC.Fleece schema => schema DisPerformer
disPerformerSchema =
  FC.coerceSchema FC.boolean