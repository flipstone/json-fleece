{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.CharacterBase.Name
  ( Name(..)
  , nameSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Name = Name T.Text
  deriving (Show, Eq)

nameSchema :: FC.Fleece schema => schema Name
nameSchema =
  FC.coerceSchema FC.text