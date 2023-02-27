{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ResponsePage.TotalElements
  ( TotalElements(..)
  , totalElementsSchema
  ) where

import Data.Int (Int32)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype TotalElements = TotalElements Int32
  deriving (Show, Eq)

totalElementsSchema :: FC.Fleece schema => schema TotalElements
totalElementsSchema =
  FC.coerceSchema FC.int32