{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CharacterFull.SerialNumber
  ( SerialNumber(..)
  , serialNumberSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype SerialNumber = SerialNumber Text
  deriving (Show, Eq)

serialNumberSchema :: FC.Fleece schema => schema SerialNumber
serialNumberSchema =
  FC.coerceSchema FC.text