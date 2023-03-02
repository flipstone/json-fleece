{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ElementFull.Symbol
  ( Symbol(..)
  , symbolSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Symbol = Symbol T.Text
  deriving (Show, Eq)

symbolSchema :: FC.Fleece schema => schema Symbol
symbolSchema =
  FC.coerceSchema FC.text