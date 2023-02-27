{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpacecraftFull.Status
  ( Status(..)
  , statusSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Status = Status Text
  deriving (Show, Eq)

statusSchema :: FC.Fleece schema => schema Status
statusSchema =
  FC.coerceSchema FC.text