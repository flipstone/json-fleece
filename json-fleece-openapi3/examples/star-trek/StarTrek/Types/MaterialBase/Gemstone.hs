{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MaterialBase.Gemstone
  ( Gemstone(..)
  , gemstoneSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Gemstone = Gemstone Bool
  deriving (Show, Eq)

gemstoneSchema :: FC.Fleece t => FC.Schema t Gemstone
gemstoneSchema =
  FC.coerceSchema FC.boolean