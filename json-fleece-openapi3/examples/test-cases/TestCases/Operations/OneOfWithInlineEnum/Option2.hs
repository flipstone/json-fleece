{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.OneOfWithInlineEnum.Option2
  ( Option2(..)
  , option2Schema
  , option2ToText
  , option2FromText
  ) where

import qualified Data.Either as Either
import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (($), (<>), Bounded, Either, Enum, Eq, Ord, Show, String)

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

option2FromText :: T.Text -> Either String Option2
option2FromText txt =
  case T.unpack txt of
    "uno" -> Either.Right Uno
    "dos" -> Either.Right Dos
    "tres" -> Either.Right Tres
    v -> Either.Left $ "Unknown Option2: " <> v

option2Schema :: FC.Fleece t => FC.Schema t Option2
option2Schema =
  FC.boundedEnum option2ToText