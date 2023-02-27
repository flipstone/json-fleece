{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ConflictFull.Name
  ( Name(..)
  , nameSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Name = Name Text
  deriving (Show, Eq)

nameSchema :: FC.Fleece schema => schema Name
nameSchema =
  FC.coerceSchema FC.text