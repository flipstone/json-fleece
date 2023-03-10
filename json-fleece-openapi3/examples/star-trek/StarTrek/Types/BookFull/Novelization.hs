{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.BookFull.Novelization
  ( Novelization(..)
  , novelizationSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Novelization = Novelization Bool
  deriving (Show, Eq)

novelizationSchema :: FC.Fleece schema => schema Novelization
novelizationSchema =
  FC.coerceSchema FC.boolean