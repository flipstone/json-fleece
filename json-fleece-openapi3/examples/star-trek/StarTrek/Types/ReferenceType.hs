{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ReferenceType
  ( ReferenceType(..)
  , referenceTypeSchema
  , referenceTypeToText
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (($), Bounded, Enum, Eq, Ord, Show)

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

referenceTypeSchema :: FC.Fleece t => FC.Schema t ReferenceType
referenceTypeSchema =
  FC.boundedEnum referenceTypeToText