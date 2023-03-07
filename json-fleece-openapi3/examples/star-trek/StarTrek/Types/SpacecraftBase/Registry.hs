{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SpacecraftBase.Registry
  ( Registry(..)
  , registrySchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Registry = Registry T.Text
  deriving (Show, Eq)

registrySchema :: FC.Fleece schema => schema Registry
registrySchema =
  FC.coerceSchema FC.text