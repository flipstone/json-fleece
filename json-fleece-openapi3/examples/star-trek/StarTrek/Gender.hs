{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Gender
  ( Gender(..)
  , genderSchema
  ) where

import Data.Text (Text, pack)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (($), Bounded, Enum, Eq, Ord, Show)

data Gender
  = F
  | M
  deriving (Eq, Show, Ord, Enum, Bounded)

genderToText :: Gender -> Text
genderToText v =
  pack $
    case v of
      F -> "F"
      M -> "M"

genderSchema :: FC.Fleece schema => schema Gender
genderSchema =
  FC.boundedEnum genderToText