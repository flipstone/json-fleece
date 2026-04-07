{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MaritalStatus
  ( MaritalStatus(..)
  , maritalStatusSchema
  , maritalStatusToText
  , maritalStatusFromText
  ) where

import qualified Data.Either as Either
import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (($), (<>), Bounded, Enum, Eq, Ord, Show, String)

data MaritalStatus
  = SINGLE
  | ENGAGED
  | MARRIED
  | DIVORCED
  | REMARRIED
  | SEPARATED
  | WIDOWED
  | CAPTAINSWOMAN
  deriving (Eq, Show, Ord, Enum, Bounded)

maritalStatusToText :: MaritalStatus -> T.Text
maritalStatusToText v =
  T.pack $
    case v of
      SINGLE -> "SINGLE"
      ENGAGED -> "ENGAGED"
      MARRIED -> "MARRIED"
      DIVORCED -> "DIVORCED"
      REMARRIED -> "REMARRIED"
      SEPARATED -> "SEPARATED"
      WIDOWED -> "WIDOWED"
      CAPTAINSWOMAN -> "CAPTAINS_WOMAN"

maritalStatusFromText :: T.Text -> Either.Either String MaritalStatus
maritalStatusFromText txt =
  case T.unpack txt of
    "SINGLE" -> Either.Right SINGLE
    "ENGAGED" -> Either.Right ENGAGED
    "MARRIED" -> Either.Right MARRIED
    "DIVORCED" -> Either.Right DIVORCED
    "REMARRIED" -> Either.Right REMARRIED
    "SEPARATED" -> Either.Right SEPARATED
    "WIDOWED" -> Either.Right WIDOWED
    "CAPTAINS_WOMAN" -> Either.Right CAPTAINSWOMAN
    v -> Either.Left $ "Unknown MaritalStatus: " <> v

maritalStatusSchema :: FC.Fleece t => FC.Schema t MaritalStatus
maritalStatusSchema =
  FC.boundedEnum maritalStatusToText