{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffFull.AudioAuthor
  ( AudioAuthor(..)
  , audioAuthorSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype AudioAuthor = AudioAuthor Bool
  deriving (Show, Eq)

audioAuthorSchema :: FC.Fleece t => FC.Schema t AudioAuthor
audioAuthorSchema =
  FC.coerceSchema FC.boolean