{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TechnologyBase.IdentificationTechnology
  ( IdentificationTechnology(..)
  , identificationTechnologySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype IdentificationTechnology = IdentificationTechnology Bool
  deriving (Show, Eq)

identificationTechnologySchema :: FC.Fleece schema => schema IdentificationTechnology
identificationTechnologySchema =
  FC.coerceSchema FC.boolean