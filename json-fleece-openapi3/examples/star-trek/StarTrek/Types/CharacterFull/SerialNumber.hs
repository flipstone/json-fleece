{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.CharacterFull.SerialNumber
  ( SerialNumber(..)
  , serialNumberSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype SerialNumber = SerialNumber T.Text
  deriving (Show, Eq)

serialNumberSchema :: FC.Fleece schema => schema SerialNumber
serialNumberSchema =
  FC.coerceSchema FC.text