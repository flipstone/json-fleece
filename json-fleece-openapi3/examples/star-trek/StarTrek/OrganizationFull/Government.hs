{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.OrganizationFull.Government
  ( Government(..)
  , governmentSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Government = Government Bool
  deriving (Show, Eq)

governmentSchema :: FC.Fleece schema => schema Government
governmentSchema =
  FC.coerceSchema FC.boolean