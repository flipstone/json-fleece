{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.AnimalBase.Feline
  ( Feline(..)
  , felineSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Feline = Feline Bool
  deriving (Show, Eq)

felineSchema :: FC.Fleece t => FC.Schema t Feline
felineSchema =
  FC.coerceSchema FC.boolean