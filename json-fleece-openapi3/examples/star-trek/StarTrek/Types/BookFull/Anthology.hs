{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.BookFull.Anthology
  ( Anthology(..)
  , anthologySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Anthology = Anthology Bool
  deriving (Show, Eq)

anthologySchema :: FC.Fleece t => FC.Schema t Anthology
anthologySchema =
  FC.coerceSchema FC.boolean