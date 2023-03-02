{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookFull.RolePlayingBook
  ( RolePlayingBook(..)
  , rolePlayingBookSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype RolePlayingBook = RolePlayingBook Bool
  deriving (Show, Eq)

rolePlayingBookSchema :: FC.Fleece schema => schema RolePlayingBook
rolePlayingBookSchema =
  FC.coerceSchema FC.boolean