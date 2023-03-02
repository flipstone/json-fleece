{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpacecraftClassFull.ActiveFrom
  ( ActiveFrom(..)
  , activeFromSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype ActiveFrom = ActiveFrom T.Text
  deriving (Show, Eq)

activeFromSchema :: FC.Fleece schema => schema ActiveFrom
activeFromSchema =
  FC.coerceSchema FC.text