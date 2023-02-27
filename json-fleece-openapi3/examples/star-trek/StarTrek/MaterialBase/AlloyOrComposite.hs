{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MaterialBase.AlloyOrComposite
  ( AlloyOrComposite(..)
  , alloyOrCompositeSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype AlloyOrComposite = AlloyOrComposite Bool
  deriving (Show, Eq)

alloyOrCompositeSchema :: FC.Fleece schema => schema AlloyOrComposite
alloyOrCompositeSchema =
  FC.coerceSchema FC.boolean