{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.BloodType
  ( BloodType(..)
  , bloodTypeSchema
  , bloodTypeToText
  , bloodTypeFromText
  ) where

import qualified Data.Either as Either
import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (($), (<>), Bounded, Enum, Eq, Ord, Show, String)

data BloodType
  = BNEGATIVE
  | ONEGATIVE
  | TNEGATIVE
  deriving (Eq, Show, Ord, Enum, Bounded)

bloodTypeToText :: BloodType -> T.Text
bloodTypeToText v =
  T.pack $
    case v of
      BNEGATIVE -> "B_NEGATIVE"
      ONEGATIVE -> "O_NEGATIVE"
      TNEGATIVE -> "T_NEGATIVE"

bloodTypeFromText :: T.Text -> Either.Either String BloodType
bloodTypeFromText txt =
  case T.unpack txt of
    "B_NEGATIVE" -> Either.Right BNEGATIVE
    "O_NEGATIVE" -> Either.Right ONEGATIVE
    "T_NEGATIVE" -> Either.Right TNEGATIVE
    v -> Either.Left $ "Unknown BloodType: " <> v

bloodTypeSchema :: FC.Fleece t => FC.Schema t BloodType
bloodTypeSchema =
  FC.boundedEnum bloodTypeToText