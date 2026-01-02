{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TechnologyFull.ComputerTechnology
  ( ComputerTechnology(..)
  , computerTechnologySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ComputerTechnology = ComputerTechnology Bool
  deriving (Show, Eq)

computerTechnologySchema :: FC.Fleece t => FC.Schema t ComputerTechnology
computerTechnologySchema =
  FC.coerceSchema FC.boolean