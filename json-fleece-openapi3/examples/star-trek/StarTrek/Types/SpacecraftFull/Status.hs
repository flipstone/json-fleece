{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SpacecraftFull.Status
  ( Status(..)
  , statusSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Status = Status T.Text
  deriving (Show, Eq)

statusSchema :: FC.Fleece schema => schema Status
statusSchema =
  FC.coerceSchema FC.text