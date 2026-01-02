{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.CharacterBase.SerialNumber
  ( SerialNumber(..)
  , serialNumberSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype SerialNumber = SerialNumber T.Text
  deriving (Show, Eq)

serialNumberSchema :: FC.Fleece t => FC.Schema t SerialNumber
serialNumberSchema =
  FC.coerceSchema FC.text