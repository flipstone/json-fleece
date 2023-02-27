{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.PerformerBase.TosPerformer
  ( TosPerformer(..)
  , tosPerformerSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype TosPerformer = TosPerformer Bool
  deriving (Show, Eq)

tosPerformerSchema :: FC.Fleece schema => schema TosPerformer
tosPerformerSchema =
  FC.coerceSchema FC.boolean