{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ResponsePage.NumberOfElements
  ( NumberOfElements(..)
  , numberOfElementsSchema
  ) where

import Data.Int (Int32)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype NumberOfElements = NumberOfElements Int32
  deriving (Show, Eq)

numberOfElementsSchema :: FC.Fleece schema => schema NumberOfElements
numberOfElementsSchema =
  FC.coerceSchema FC.int32