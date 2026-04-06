{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.Gender
  ( Gender(..)
  , genderSchema
  , genderToText
  , genderFromText
  ) where

import qualified Data.Either as Either
import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (($), (<>), Bounded, Either, Enum, Eq, Ord, Show, String)

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

genderFromText :: T.Text -> Either String Gender
genderFromText txt =
  case T.unpack txt of
    "F" -> Either.Right F
    "M" -> Either.Right M
    v -> Either.Left $ "Unknown Gender: " <> v

genderSchema :: FC.Fleece t => FC.Schema t Gender
genderSchema =
  FC.boundedEnum genderToText