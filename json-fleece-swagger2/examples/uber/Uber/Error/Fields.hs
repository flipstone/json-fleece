{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Error.Fields
  ( Fields(..)
  , fieldsSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Fields = Fields Text
  deriving (Show, Eq)

fieldsSchema :: FC.Fleece schema => schema Fields
fieldsSchema =
  FC.coerceSchema FC.text