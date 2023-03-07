{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TechnologyBase.ComputerTechnology
  ( ComputerTechnology(..)
  , computerTechnologySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ComputerTechnology = ComputerTechnology Bool
  deriving (Show, Eq)

computerTechnologySchema :: FC.Fleece schema => schema ComputerTechnology
computerTechnologySchema =
  FC.coerceSchema FC.boolean