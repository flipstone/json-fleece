{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpacecraftClassBase.ActiveFrom
  ( ActiveFrom(..)
  , activeFromSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype ActiveFrom = ActiveFrom Text
  deriving (Show, Eq)

activeFromSchema :: FC.Fleece schema => schema ActiveFrom
activeFromSchema =
  FC.coerceSchema FC.text