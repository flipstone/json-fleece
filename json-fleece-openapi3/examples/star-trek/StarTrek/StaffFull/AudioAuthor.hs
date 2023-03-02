{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffFull.AudioAuthor
  ( AudioAuthor(..)
  , audioAuthorSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype AudioAuthor = AudioAuthor Bool
  deriving (Show, Eq)

audioAuthorSchema :: FC.Fleece schema => schema AudioAuthor
audioAuthorSchema =
  FC.coerceSchema FC.boolean