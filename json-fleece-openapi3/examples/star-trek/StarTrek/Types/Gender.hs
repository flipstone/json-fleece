{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.Gender
  ( Gender(..)
  , genderSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (($), Bounded, Enum, Eq, Ord, Show)

data Gender
  = F
  | M
  deriving (Eq, Show, Ord, Enum, Bounded)

genderToText :: Gender -> T.Text
genderToText v =
  T.pack $
    case v of
      F -> "F"
      M -> "M"

genderSchema :: FC.Fleece schema => schema Gender
genderSchema =
  FC.boundedEnum genderToText