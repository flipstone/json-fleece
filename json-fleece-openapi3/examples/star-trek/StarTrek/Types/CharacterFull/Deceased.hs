{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.CharacterFull.Deceased
  ( Deceased(..)
  , deceasedSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Deceased = Deceased Bool
  deriving (Show, Eq)

deceasedSchema :: FC.Fleece t => FC.Schema t Deceased
deceasedSchema =
  FC.coerceSchema FC.boolean