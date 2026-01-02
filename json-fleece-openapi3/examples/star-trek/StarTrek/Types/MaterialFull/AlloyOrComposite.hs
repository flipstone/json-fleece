{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MaterialFull.AlloyOrComposite
  ( AlloyOrComposite(..)
  , alloyOrCompositeSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype AlloyOrComposite = AlloyOrComposite Bool
  deriving (Show, Eq)

alloyOrCompositeSchema :: FC.Fleece t => FC.Schema t AlloyOrComposite
alloyOrCompositeSchema =
  FC.coerceSchema FC.boolean