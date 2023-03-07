{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.CompanyFull.Distributor
  ( Distributor(..)
  , distributorSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Distributor = Distributor Bool
  deriving (Show, Eq)

distributorSchema :: FC.Fleece schema => schema Distributor
distributorSchema =
  FC.coerceSchema FC.boolean