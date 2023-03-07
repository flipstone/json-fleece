{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Types.Error.Fields
  ( Fields(..)
  , fieldsSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Fields = Fields T.Text
  deriving (Show, Eq)

fieldsSchema :: FC.Fleece schema => schema Fields
fieldsSchema =
  FC.coerceSchema FC.text