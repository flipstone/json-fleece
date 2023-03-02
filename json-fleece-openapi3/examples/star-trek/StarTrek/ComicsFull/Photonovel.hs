{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicsFull.Photonovel
  ( Photonovel(..)
  , photonovelSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Photonovel = Photonovel Bool
  deriving (Show, Eq)

photonovelSchema :: FC.Fleece schema => schema Photonovel
photonovelSchema =
  FC.coerceSchema FC.boolean