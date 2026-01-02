{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.LocationFull.SubnationalEntity
  ( SubnationalEntity(..)
  , subnationalEntitySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype SubnationalEntity = SubnationalEntity Bool
  deriving (Show, Eq)

subnationalEntitySchema :: FC.Fleece t => FC.Schema t SubnationalEntity
subnationalEntitySchema =
  FC.coerceSchema FC.boolean