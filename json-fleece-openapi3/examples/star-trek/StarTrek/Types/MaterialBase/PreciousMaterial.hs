{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MaterialBase.PreciousMaterial
  ( PreciousMaterial(..)
  , preciousMaterialSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype PreciousMaterial = PreciousMaterial Bool
  deriving (Show, Eq)

preciousMaterialSchema :: FC.Fleece t => FC.Schema t PreciousMaterial
preciousMaterialSchema =
  FC.coerceSchema FC.boolean