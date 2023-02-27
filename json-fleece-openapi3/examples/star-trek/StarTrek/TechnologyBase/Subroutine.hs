{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TechnologyBase.Subroutine
  ( Subroutine(..)
  , subroutineSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Subroutine = Subroutine Bool
  deriving (Show, Eq)

subroutineSchema :: FC.Fleece schema => schema Subroutine
subroutineSchema =
  FC.coerceSchema FC.boolean