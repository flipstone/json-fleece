{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.AnimalBase.Avian
  ( Avian(..)
  , avianSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Avian = Avian Bool
  deriving (Show, Eq)

avianSchema :: FC.Fleece t => FC.Schema t Avian
avianSchema =
  FC.coerceSchema FC.boolean