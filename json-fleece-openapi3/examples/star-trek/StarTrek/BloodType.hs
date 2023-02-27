{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BloodType
  ( BloodType(..)
  , bloodTypeSchema
  ) where

import Data.Text (Text, pack)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (($), Bounded, Enum, Eq, Ord, Show)

data BloodType
  = BNEGATIVE
  | ONEGATIVE
  | TNEGATIVE
  deriving (Eq, Show, Ord, Enum, Bounded)

bloodTypeToText :: BloodType -> Text
bloodTypeToText v =
  pack $
    case v of
      BNEGATIVE -> "B_NEGATIVE"
      ONEGATIVE -> "O_NEGATIVE"
      TNEGATIVE -> "T_NEGATIVE"

bloodTypeSchema :: FC.Fleece schema => schema BloodType
bloodTypeSchema =
  FC.boundedEnum bloodTypeToText