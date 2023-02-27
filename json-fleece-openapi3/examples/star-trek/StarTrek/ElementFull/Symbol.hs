{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ElementFull.Symbol
  ( Symbol(..)
  , symbolSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Symbol = Symbol Text
  deriving (Show, Eq)

symbolSchema :: FC.Fleece schema => schema Symbol
symbolSchema =
  FC.coerceSchema FC.text