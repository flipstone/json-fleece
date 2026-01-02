{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.Error.Message
  ( Message(..)
  , messageSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Message = Message T.Text
  deriving (Show, Eq)

messageSchema :: FC.Fleece t => FC.Schema t Message
messageSchema =
  FC.coerceSchema FC.text