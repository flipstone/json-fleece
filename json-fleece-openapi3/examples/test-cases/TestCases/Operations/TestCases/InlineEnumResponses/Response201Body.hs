{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.TestCases.InlineEnumResponses.Response201Body
  ( Response201Body(..)
  , response201BodySchema
  , response201BodyToText
  , response201BodyFromText
  ) where

import qualified Data.Either as Either
import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (($), (<>), Bounded, Either, Enum, Eq, Ord, Show, String)

data Response201Body
  = Baz
  | Bat
  deriving (Eq, Show, Ord, Enum, Bounded)

response201BodyToText :: Response201Body -> T.Text
response201BodyToText v =
  T.pack $
    case v of
      Baz -> "baz"
      Bat -> "bat"

response201BodyFromText :: T.Text -> Either String Response201Body
response201BodyFromText txt =
  case T.unpack txt of
    "baz" -> Either.Right Baz
    "bat" -> Either.Right Bat
    v -> Either.Left $ "Unknown Response201Body: " <> v

response201BodySchema :: FC.Fleece t => FC.Schema t Response201Body
response201BodySchema =
  FC.boundedEnum response201BodyToText