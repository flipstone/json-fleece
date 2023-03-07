{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SpacecraftClassBase.ActiveTo
  ( ActiveTo(..)
  , activeToSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype ActiveTo = ActiveTo T.Text
  deriving (Show, Eq)

activeToSchema :: FC.Fleece schema => schema ActiveTo
activeToSchema =
  FC.coerceSchema FC.text