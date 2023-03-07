{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Types.Profile.LastName
  ( LastName(..)
  , lastNameSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype LastName = LastName T.Text
  deriving (Show, Eq)

lastNameSchema :: FC.Fleece schema => schema LastName
lastNameSchema =
  FC.coerceSchema FC.text