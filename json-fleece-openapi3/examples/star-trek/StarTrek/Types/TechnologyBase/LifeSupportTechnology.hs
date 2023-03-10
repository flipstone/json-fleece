{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TechnologyBase.LifeSupportTechnology
  ( LifeSupportTechnology(..)
  , lifeSupportTechnologySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype LifeSupportTechnology = LifeSupportTechnology Bool
  deriving (Show, Eq)

lifeSupportTechnologySchema :: FC.Fleece schema => schema LifeSupportTechnology
lifeSupportTechnologySchema =
  FC.coerceSchema FC.boolean