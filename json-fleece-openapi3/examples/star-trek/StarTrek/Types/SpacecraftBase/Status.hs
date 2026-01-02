{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SpacecraftBase.Status
  ( Status(..)
  , statusSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Status = Status T.Text
  deriving (Show, Eq)

statusSchema :: FC.Fleece t => FC.Schema t Status
statusSchema =
  FC.coerceSchema FC.text