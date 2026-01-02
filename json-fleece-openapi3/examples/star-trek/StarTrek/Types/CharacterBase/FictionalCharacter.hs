{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.CharacterBase.FictionalCharacter
  ( FictionalCharacter(..)
  , fictionalCharacterSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype FictionalCharacter = FictionalCharacter Bool
  deriving (Show, Eq)

fictionalCharacterSchema :: FC.Fleece t => FC.Schema t FictionalCharacter
fictionalCharacterSchema =
  FC.coerceSchema FC.boolean