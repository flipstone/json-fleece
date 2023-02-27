{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ResponsePage.TotalPages
  ( TotalPages(..)
  , totalPagesSchema
  ) where

import Data.Int (Int32)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype TotalPages = TotalPages Int32
  deriving (Show, Eq)

totalPagesSchema :: FC.Fleece schema => schema TotalPages
totalPagesSchema =
  FC.coerceSchema FC.int32