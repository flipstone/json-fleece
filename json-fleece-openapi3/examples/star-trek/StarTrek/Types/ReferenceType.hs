{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ReferenceType
  ( ReferenceType(..)
  , referenceTypeSchema
  , referenceTypeToText
  , referenceTypeFromText
  ) where

import qualified Data.Either as Either
import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (($), (<>), Bounded, Either, Enum, Eq, Ord, Show, String)

data ReferenceType
  = ASIN
  | ISBN
  deriving (Eq, Show, Ord, Enum, Bounded)

referenceTypeToText :: ReferenceType -> T.Text
referenceTypeToText v =
  T.pack $
    case v of
      ASIN -> "ASIN"
      ISBN -> "ISBN"

referenceTypeFromText :: T.Text -> Either String ReferenceType
referenceTypeFromText txt =
  case T.unpack txt of
    "ASIN" -> Either.Right ASIN
    "ISBN" -> Either.Right ISBN
    v -> Either.Left $ "Unknown ReferenceType: " <> v

referenceTypeSchema :: FC.Fleece t => FC.Schema t ReferenceType
referenceTypeSchema =
  FC.boundedEnum referenceTypeToText