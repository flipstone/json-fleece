{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MaterialFull.AlloyOrComposite
  ( AlloyOrComposite(..)
  , alloyOrCompositeSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype AlloyOrComposite = AlloyOrComposite Bool
  deriving (Show, Eq)

alloyOrCompositeSchema :: FC.Fleece schema => schema AlloyOrComposite
alloyOrCompositeSchema =
  FC.coerceSchema FC.boolean