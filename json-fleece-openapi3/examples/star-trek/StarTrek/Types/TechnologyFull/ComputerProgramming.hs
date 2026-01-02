{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TechnologyFull.ComputerProgramming
  ( ComputerProgramming(..)
  , computerProgrammingSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ComputerProgramming = ComputerProgramming Bool
  deriving (Show, Eq)

computerProgrammingSchema :: FC.Fleece t => FC.Schema t ComputerProgramming
computerProgrammingSchema =
  FC.coerceSchema FC.boolean