{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.Status
  ( Status(..)
  , statusSchema
  , statusToText
  , statusFromText
  ) where

import qualified Data.Either as Either
import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (($), (<>), Bounded, Enum, Eq, Ord, Show, String)

data Status
  = GOOD
  | BAD
  | UGLY
  deriving (Eq, Show, Ord, Enum, Bounded)

statusToText :: Status -> T.Text
statusToText v =
  T.pack $
    case v of
      GOOD -> "GOOD"
      BAD -> "BAD"
      UGLY -> "UGLY"

statusFromText :: T.Text -> Either.Either String Status
statusFromText txt =
  case T.unpack txt of
    "GOOD" -> Either.Right GOOD
    "BAD" -> Either.Right BAD
    "UGLY" -> Either.Right UGLY
    v -> Either.Left $ "Unknown Status: " <> v

statusSchema :: FC.Fleece t => FC.Schema t Status
statusSchema =
  FC.boundedEnum statusToText