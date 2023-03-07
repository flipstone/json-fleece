{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.PerformerFull.TosPerformer
  ( TosPerformer(..)
  , tosPerformerSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype TosPerformer = TosPerformer Bool
  deriving (Show, Eq)

tosPerformerSchema :: FC.Fleece schema => schema TosPerformer
tosPerformerSchema =
  FC.coerceSchema FC.boolean