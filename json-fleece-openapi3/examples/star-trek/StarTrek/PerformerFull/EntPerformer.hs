{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.PerformerFull.EntPerformer
  ( EntPerformer(..)
  , entPerformerSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype EntPerformer = EntPerformer Bool
  deriving (Show, Eq)

entPerformerSchema :: FC.Fleece schema => schema EntPerformer
entPerformerSchema =
  FC.coerceSchema FC.boolean