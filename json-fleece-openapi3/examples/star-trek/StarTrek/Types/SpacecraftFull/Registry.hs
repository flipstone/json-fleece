{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SpacecraftFull.Registry
  ( Registry(..)
  , registrySchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Registry = Registry T.Text
  deriving (Show, Eq)

registrySchema :: FC.Fleece t => FC.Schema t Registry
registrySchema =
  FC.coerceSchema FC.text