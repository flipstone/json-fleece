{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffFull.NovelAuthor
  ( NovelAuthor(..)
  , novelAuthorSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype NovelAuthor = NovelAuthor Bool
  deriving (Show, Eq)

novelAuthorSchema :: FC.Fleece t => FC.Schema t NovelAuthor
novelAuthorSchema =
  FC.coerceSchema FC.boolean