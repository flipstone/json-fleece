{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MaterialBase.PreciousMaterial
  ( PreciousMaterial(..)
  , preciousMaterialSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype PreciousMaterial = PreciousMaterial Bool
  deriving (Show, Eq)

preciousMaterialSchema :: FC.Fleece schema => schema PreciousMaterial
preciousMaterialSchema =
  FC.coerceSchema FC.boolean