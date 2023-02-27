{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CompanyBase.Distributor
  ( Distributor(..)
  , distributorSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Distributor = Distributor Bool
  deriving (Show, Eq)

distributorSchema :: FC.Fleece schema => schema Distributor
distributorSchema =
  FC.coerceSchema FC.boolean