{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CharacterFull.FictionalCharacter
  ( FictionalCharacter(..)
  , fictionalCharacterSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype FictionalCharacter = FictionalCharacter Bool
  deriving (Show, Eq)

fictionalCharacterSchema :: FC.Fleece schema => schema FictionalCharacter
fictionalCharacterSchema =
  FC.coerceSchema FC.boolean