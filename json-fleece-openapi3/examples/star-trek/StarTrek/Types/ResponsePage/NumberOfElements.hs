{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ResponsePage.NumberOfElements
  ( NumberOfElements(..)
  , numberOfElementsSchema
  ) where

import qualified Data.Int as I
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype NumberOfElements = NumberOfElements I.Int32
  deriving (Show, Eq)

numberOfElementsSchema :: FC.Fleece t => FC.Schema t NumberOfElements
numberOfElementsSchema =
  FC.coerceSchema FC.int32