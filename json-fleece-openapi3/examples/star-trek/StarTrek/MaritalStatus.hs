{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MaritalStatus
  ( MaritalStatus(..)
  , maritalStatusSchema
  ) where

import Data.Text (Text, pack)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (($), Bounded, Enum, Eq, Ord, Show)

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

maritalStatusToText :: MaritalStatus -> Text
maritalStatusToText v =
  pack $
    case v of
      SINGLE -> "SINGLE"
      ENGAGED -> "ENGAGED"
      MARRIED -> "MARRIED"
      DIVORCED -> "DIVORCED"
      REMARRIED -> "REMARRIED"
      SEPARATED -> "SEPARATED"
      WIDOWED -> "WIDOWED"
      CAPTAINSWOMAN -> "CAPTAINS_WOMAN"

maritalStatusSchema :: FC.Fleece schema => schema MaritalStatus
maritalStatusSchema =
  FC.boundedEnum maritalStatusToText