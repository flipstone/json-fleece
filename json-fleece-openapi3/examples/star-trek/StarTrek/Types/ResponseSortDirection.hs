{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ResponseSortDirection
  ( ResponseSortDirection(..)
  , responseSortDirectionSchema
  , responseSortDirectionToText
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (($), Bounded, Enum, Eq, Ord, Show)

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

responseSortDirectionSchema :: FC.Fleece t => FC.Schema t ResponseSortDirection
responseSortDirectionSchema =
  FC.boundedEnum responseSortDirectionToText