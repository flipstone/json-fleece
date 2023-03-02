{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookFull.Novel
  ( Novel(..)
  , novelSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Novel = Novel Bool
  deriving (Show, Eq)

novelSchema :: FC.Fleece schema => schema Novel
novelSchema =
  FC.coerceSchema FC.boolean