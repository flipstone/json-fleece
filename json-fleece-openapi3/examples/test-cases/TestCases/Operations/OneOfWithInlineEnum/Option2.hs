{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.OneOfWithInlineEnum.Option2
  ( Option2(..)
  , option2Schema
  , option2ToText
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (($), Bounded, Enum, Eq, Ord, Show)

data Option2
  = Uno
  | Dos
  | Tres
  deriving (Eq, Show, Ord, Enum, Bounded)

option2ToText :: Option2 -> T.Text
option2ToText v =
  T.pack $
    case v of
      Uno -> "uno"
      Dos -> "dos"
      Tres -> "tres"

option2Schema :: FC.Fleece t => FC.Schema t Option2
option2Schema =
  FC.boundedEnum option2ToText