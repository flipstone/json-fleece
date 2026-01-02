{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.BookFull.Novelization
  ( Novelization(..)
  , novelizationSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Novelization = Novelization Bool
  deriving (Show, Eq)

novelizationSchema :: FC.Fleece t => FC.Schema t Novelization
novelizationSchema =
  FC.coerceSchema FC.boolean