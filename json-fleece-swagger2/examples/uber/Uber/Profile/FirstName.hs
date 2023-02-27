{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Profile.FirstName
  ( FirstName(..)
  , firstNameSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype FirstName = FirstName Text
  deriving (Show, Eq)

firstNameSchema :: FC.Fleece schema => schema FirstName
firstNameSchema =
  FC.coerceSchema FC.text