{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpacecraftFull.Registry
  ( Registry(..)
  , registrySchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Registry = Registry Text
  deriving (Show, Eq)

registrySchema :: FC.Fleece schema => schema Registry
registrySchema =
  FC.coerceSchema FC.text