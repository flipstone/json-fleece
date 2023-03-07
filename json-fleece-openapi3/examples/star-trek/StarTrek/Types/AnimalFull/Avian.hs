{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.AnimalFull.Avian
  ( Avian(..)
  , avianSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Avian = Avian Bool
  deriving (Show, Eq)

avianSchema :: FC.Fleece schema => schema Avian
avianSchema =
  FC.coerceSchema FC.boolean