{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ElementBase.Symbol
  ( Symbol(..)
  , symbolSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Symbol = Symbol T.Text
  deriving (Show, Eq)

symbolSchema :: FC.Fleece t => FC.Schema t Symbol
symbolSchema =
  FC.coerceSchema FC.text