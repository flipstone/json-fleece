{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TechnologyFull.BorgTechnology
  ( BorgTechnology(..)
  , borgTechnologySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype BorgTechnology = BorgTechnology Bool
  deriving (Show, Eq)

borgTechnologySchema :: FC.Fleece schema => schema BorgTechnology
borgTechnologySchema =
  FC.coerceSchema FC.boolean