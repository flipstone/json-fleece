{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.Error.Code
  ( Code(..)
  , codeSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Code = Code T.Text
  deriving (Show, Eq)

codeSchema :: FC.Fleece t => FC.Schema t Code
codeSchema =
  FC.coerceSchema FC.text