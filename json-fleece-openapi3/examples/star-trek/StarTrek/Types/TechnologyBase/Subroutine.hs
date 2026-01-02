{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TechnologyBase.Subroutine
  ( Subroutine(..)
  , subroutineSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Subroutine = Subroutine Bool
  deriving (Show, Eq)

subroutineSchema :: FC.Fleece t => FC.Schema t Subroutine
subroutineSchema =
  FC.coerceSchema FC.boolean