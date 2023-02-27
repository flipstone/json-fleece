{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CharacterBase.Deceased
  ( Deceased(..)
  , deceasedSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Deceased = Deceased Bool
  deriving (Show, Eq)

deceasedSchema :: FC.Fleece schema => schema Deceased
deceasedSchema =
  FC.coerceSchema FC.boolean