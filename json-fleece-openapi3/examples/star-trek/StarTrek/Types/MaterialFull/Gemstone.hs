{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MaterialFull.Gemstone
  ( Gemstone(..)
  , gemstoneSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Gemstone = Gemstone Bool
  deriving (Show, Eq)

gemstoneSchema :: FC.Fleece schema => schema Gemstone
gemstoneSchema =
  FC.coerceSchema FC.boolean