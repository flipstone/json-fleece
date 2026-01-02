{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.BookBase.RolePlayingBook
  ( RolePlayingBook(..)
  , rolePlayingBookSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype RolePlayingBook = RolePlayingBook Bool
  deriving (Show, Eq)

rolePlayingBookSchema :: FC.Fleece t => FC.Schema t RolePlayingBook
rolePlayingBookSchema =
  FC.coerceSchema FC.boolean