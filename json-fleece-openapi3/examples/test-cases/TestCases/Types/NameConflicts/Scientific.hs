{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.NameConflicts.Scientific
  ( Scientific(..)
  , scientificSchema
  ) where

import qualified Data.Scientific as Sci
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Scientific = Scientific Sci.Scientific
  deriving (Show, Eq)

scientificSchema :: FC.Fleece schema => schema Scientific
scientificSchema =
  FC.coerceSchema FC.number