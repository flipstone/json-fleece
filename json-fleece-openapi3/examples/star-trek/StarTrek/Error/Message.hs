{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Error.Message
  ( Message(..)
  , messageSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Message = Message Text
  deriving (Show, Eq)

messageSchema :: FC.Fleece schema => schema Message
messageSchema =
  FC.coerceSchema FC.text