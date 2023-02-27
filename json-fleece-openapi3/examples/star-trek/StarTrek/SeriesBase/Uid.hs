{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SeriesBase.Uid
  ( Uid(..)
  , uidSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Uid = Uid Text
  deriving (Show, Eq)

uidSchema :: FC.Fleece schema => schema Uid
uidSchema =
  FC.coerceSchema FC.text