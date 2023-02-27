{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.AnimalBase.Avian
  ( Avian(..)
  , avianSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Avian = Avian Bool
  deriving (Show, Eq)

avianSchema :: FC.Fleece schema => schema Avian
avianSchema =
  FC.coerceSchema FC.boolean