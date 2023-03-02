{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookFull.Anthology
  ( Anthology(..)
  , anthologySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Anthology = Anthology Bool
  deriving (Show, Eq)

anthologySchema :: FC.Fleece schema => schema Anthology
anthologySchema =
  FC.coerceSchema FC.boolean