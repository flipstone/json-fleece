{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TechnologyBase.BorgTechnology
  ( BorgTechnology(..)
  , borgTechnologySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype BorgTechnology = BorgTechnology Bool
  deriving (Show, Eq)

borgTechnologySchema :: FC.Fleece t => FC.Schema t BorgTechnology
borgTechnologySchema =
  FC.coerceSchema FC.boolean