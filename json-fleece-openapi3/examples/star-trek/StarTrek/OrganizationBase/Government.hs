{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.OrganizationBase.Government
  ( Government(..)
  , governmentSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Government = Government Bool
  deriving (Show, Eq)

governmentSchema :: FC.Fleece schema => schema Government
governmentSchema =
  FC.coerceSchema FC.boolean