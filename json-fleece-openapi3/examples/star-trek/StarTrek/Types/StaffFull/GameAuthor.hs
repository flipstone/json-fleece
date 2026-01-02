{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffFull.GameAuthor
  ( GameAuthor(..)
  , gameAuthorSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype GameAuthor = GameAuthor Bool
  deriving (Show, Eq)

gameAuthorSchema :: FC.Fleece t => FC.Schema t GameAuthor
gameAuthorSchema =
  FC.coerceSchema FC.boolean