{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Profile.Email
  ( Email(..)
  , emailSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Email = Email Text
  deriving (Show, Eq)

emailSchema :: FC.Fleece schema => schema Email
emailSchema =
  FC.coerceSchema FC.text