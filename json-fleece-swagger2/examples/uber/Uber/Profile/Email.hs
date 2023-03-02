{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Profile.Email
  ( Email(..)
  , emailSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Email = Email T.Text
  deriving (Show, Eq)

emailSchema :: FC.Fleece schema => schema Email
emailSchema =
  FC.coerceSchema FC.text