{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TechnologyBase.ComputerProgramming
  ( ComputerProgramming(..)
  , computerProgrammingSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ComputerProgramming = ComputerProgramming Bool
  deriving (Show, Eq)

computerProgrammingSchema :: FC.Fleece schema => schema ComputerProgramming
computerProgrammingSchema =
  FC.coerceSchema FC.boolean