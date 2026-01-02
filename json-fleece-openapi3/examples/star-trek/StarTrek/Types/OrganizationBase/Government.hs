{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.OrganizationBase.Government
  ( Government(..)
  , governmentSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Government = Government Bool
  deriving (Show, Eq)

governmentSchema :: FC.Fleece t => FC.Schema t Government
governmentSchema =
  FC.coerceSchema FC.boolean