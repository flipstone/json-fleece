{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.FieldDescriptions.NoDescription
  ( NoDescription(..)
  , noDescriptionSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype NoDescription = NoDescription T.Text
  deriving (Show, Eq)

noDescriptionSchema :: FC.Fleece t => FC.Schema t NoDescription
noDescriptionSchema =
  FC.coerceSchema FC.text