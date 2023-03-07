{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffBase.GameAuthor
  ( GameAuthor(..)
  , gameAuthorSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype GameAuthor = GameAuthor Bool
  deriving (Show, Eq)

gameAuthorSchema :: FC.Fleece schema => schema GameAuthor
gameAuthorSchema =
  FC.coerceSchema FC.boolean