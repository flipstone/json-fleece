{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpacecraftClassBase.ActiveTo
  ( ActiveTo(..)
  , activeToSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype ActiveTo = ActiveTo Text
  deriving (Show, Eq)

activeToSchema :: FC.Fleece schema => schema ActiveTo
activeToSchema =
  FC.coerceSchema FC.text