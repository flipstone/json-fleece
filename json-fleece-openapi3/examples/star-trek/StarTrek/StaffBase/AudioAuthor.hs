{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffBase.AudioAuthor
  ( AudioAuthor(..)
  , audioAuthorSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype AudioAuthor = AudioAuthor Bool
  deriving (Show, Eq)

audioAuthorSchema :: FC.Fleece schema => schema AudioAuthor
audioAuthorSchema =
  FC.coerceSchema FC.boolean