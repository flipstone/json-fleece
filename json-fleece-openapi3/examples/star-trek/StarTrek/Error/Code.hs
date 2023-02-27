{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Error.Code
  ( Code(..)
  , codeSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Code = Code Text
  deriving (Show, Eq)

codeSchema :: FC.Fleece schema => schema Code
codeSchema =
  FC.coerceSchema FC.text