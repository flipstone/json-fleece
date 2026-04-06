{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ResponseSortDirection
  ( ResponseSortDirection(..)
  , responseSortDirectionSchema
  , responseSortDirectionToText
  , responseSortDirectionFromText
  ) where

import qualified Data.Either as Either
import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (($), (<>), Bounded, Either, Enum, Eq, Ord, Show, String)

data ResponseSortDirection
  = ASC
  | DESC
  deriving (Eq, Show, Ord, Enum, Bounded)

responseSortDirectionToText :: ResponseSortDirection -> T.Text
responseSortDirectionToText v =
  T.pack $
    case v of
      ASC -> "ASC"
      DESC -> "DESC"

responseSortDirectionFromText :: T.Text -> Either String ResponseSortDirection
responseSortDirectionFromText txt =
  case T.unpack txt of
    "ASC" -> Either.Right ASC
    "DESC" -> Either.Right DESC
    v -> Either.Left $ "Unknown ResponseSortDirection: " <> v

responseSortDirectionSchema :: FC.Fleece t => FC.Schema t ResponseSortDirection
responseSortDirectionSchema =
  FC.boundedEnum responseSortDirectionToText