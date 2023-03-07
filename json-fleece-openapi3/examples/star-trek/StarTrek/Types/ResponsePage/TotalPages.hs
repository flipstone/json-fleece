{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ResponsePage.TotalPages
  ( TotalPages(..)
  , totalPagesSchema
  ) where

import qualified Data.Int as I
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype TotalPages = TotalPages I.Int32
  deriving (Show, Eq)

totalPagesSchema :: FC.Fleece schema => schema TotalPages
totalPagesSchema =
  FC.coerceSchema FC.int32