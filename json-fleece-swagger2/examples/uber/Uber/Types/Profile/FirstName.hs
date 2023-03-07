{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Types.Profile.FirstName
  ( FirstName(..)
  , firstNameSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype FirstName = FirstName T.Text
  deriving (Show, Eq)

firstNameSchema :: FC.Fleece schema => schema FirstName
firstNameSchema =
  FC.coerceSchema FC.text