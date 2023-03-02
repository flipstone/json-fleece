{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ResponsePage.TotalElements
  ( TotalElements(..)
  , totalElementsSchema
  ) where

import qualified Data.Int as I
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype TotalElements = TotalElements I.Int32
  deriving (Show, Eq)

totalElementsSchema :: FC.Fleece schema => schema TotalElements
totalElementsSchema =
  FC.coerceSchema FC.int32