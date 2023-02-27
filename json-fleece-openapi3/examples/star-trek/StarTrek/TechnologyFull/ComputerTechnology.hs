{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TechnologyFull.ComputerTechnology
  ( ComputerTechnology(..)
  , computerTechnologySchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ComputerTechnology = ComputerTechnology Bool
  deriving (Show, Eq)

computerTechnologySchema :: FC.Fleece schema => schema ComputerTechnology
computerTechnologySchema =
  FC.coerceSchema FC.boolean