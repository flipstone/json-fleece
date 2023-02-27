{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Profile.LastName
  ( LastName(..)
  , lastNameSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype LastName = LastName Text
  deriving (Show, Eq)

lastNameSchema :: FC.Fleece schema => schema LastName
lastNameSchema =
  FC.coerceSchema FC.text