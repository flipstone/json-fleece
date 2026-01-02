{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ResponsePage.TotalElements
  ( TotalElements(..)
  , totalElementsSchema
  ) where

import qualified Data.Int as I
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype TotalElements = TotalElements I.Int32
  deriving (Show, Eq)

totalElementsSchema :: FC.Fleece t => FC.Schema t TotalElements
totalElementsSchema =
  FC.coerceSchema FC.int32