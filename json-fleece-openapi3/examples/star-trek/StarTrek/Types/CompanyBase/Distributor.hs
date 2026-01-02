{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.CompanyBase.Distributor
  ( Distributor(..)
  , distributorSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Distributor = Distributor Bool
  deriving (Show, Eq)

distributorSchema :: FC.Fleece t => FC.Schema t Distributor
distributorSchema =
  FC.coerceSchema FC.boolean