{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.BloodType
  ( BloodType(..)
  , bloodTypeSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (($), Bounded, Enum, Eq, Ord, Show)

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

bloodTypeSchema :: FC.Fleece schema => schema BloodType
bloodTypeSchema =
  FC.boundedEnum bloodTypeToText