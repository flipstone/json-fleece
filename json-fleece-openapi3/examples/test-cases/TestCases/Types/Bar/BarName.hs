{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.Bar.BarName
  ( BarName(..)
  , barNameSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype BarName = BarName T.Text
  deriving (Show, Eq)

barNameSchema :: FC.Fleece schema => schema BarName
barNameSchema =
  FC.coerceSchema FC.text