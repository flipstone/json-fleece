{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Error.Code
  ( Code(..)
  , codeSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Code = Code T.Text
  deriving (Show, Eq)

codeSchema :: FC.Fleece schema => schema Code
codeSchema =
  FC.coerceSchema FC.text