{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffBase.GameAuthor
  ( GameAuthor(..)
  , gameAuthorSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype GameAuthor = GameAuthor Bool
  deriving (Show, Eq)

gameAuthorSchema :: FC.Fleece schema => schema GameAuthor
gameAuthorSchema =
  FC.coerceSchema FC.boolean