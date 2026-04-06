{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ContentRatingSystem
  ( ContentRatingSystem(..)
  , contentRatingSystemSchema
  , contentRatingSystemToText
  , contentRatingSystemFromText
  ) where

import qualified Data.Either as Either
import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (($), (<>), Bounded, Either, Enum, Eq, Ord, Show, String)

data ContentRatingSystem
  = BBFC
  | OFLC
  | OFLCNZ
  | DJCTQ
  | MDA
  | MPAA
  | CHVRS
  | RCQ
  | IFCO
  | FSK
  | NICAM
  | MCCYP
  | EIRIN
  | HK
  | CBFC
  | NMHH
  | VRC
  | RSAC
  | ESRB
  | ELSPA
  | PEGI
  | USK
  | SELL
  | ADESE
  | GSRR
  | ITUNES
  deriving (Eq, Show, Ord, Enum, Bounded)

contentRatingSystemToText :: ContentRatingSystem -> T.Text
contentRatingSystemToText v =
  T.pack $
    case v of
      BBFC -> "BBFC"
      OFLC -> "OFLC"
      OFLCNZ -> "OFLCNZ"
      DJCTQ -> "DJCTQ"
      MDA -> "MDA"
      MPAA -> "MPAA"
      CHVRS -> "CHVRS"
      RCQ -> "RCQ"
      IFCO -> "IFCO"
      FSK -> "FSK"
      NICAM -> "NICAM"
      MCCYP -> "MCCYP"
      EIRIN -> "EIRIN"
      HK -> "HK"
      CBFC -> "CBFC"
      NMHH -> "NMHH"
      VRC -> "VRC"
      RSAC -> "RSAC"
      ESRB -> "ESRB"
      ELSPA -> "ELSPA"
      PEGI -> "PEGI"
      USK -> "USK"
      SELL -> "SELL"
      ADESE -> "ADESE"
      GSRR -> "GSRR"
      ITUNES -> "ITUNES"

contentRatingSystemFromText :: T.Text -> Either String ContentRatingSystem
contentRatingSystemFromText txt =
  case T.unpack txt of
    "BBFC" -> Either.Right BBFC
    "OFLC" -> Either.Right OFLC
    "OFLCNZ" -> Either.Right OFLCNZ
    "DJCTQ" -> Either.Right DJCTQ
    "MDA" -> Either.Right MDA
    "MPAA" -> Either.Right MPAA
    "CHVRS" -> Either.Right CHVRS
    "RCQ" -> Either.Right RCQ
    "IFCO" -> Either.Right IFCO
    "FSK" -> Either.Right FSK
    "NICAM" -> Either.Right NICAM
    "MCCYP" -> Either.Right MCCYP
    "EIRIN" -> Either.Right EIRIN
    "HK" -> Either.Right HK
    "CBFC" -> Either.Right CBFC
    "NMHH" -> Either.Right NMHH
    "VRC" -> Either.Right VRC
    "RSAC" -> Either.Right RSAC
    "ESRB" -> Either.Right ESRB
    "ELSPA" -> Either.Right ELSPA
    "PEGI" -> Either.Right PEGI
    "USK" -> Either.Right USK
    "SELL" -> Either.Right SELL
    "ADESE" -> Either.Right ADESE
    "GSRR" -> Either.Right GSRR
    "ITUNES" -> Either.Right ITUNES
    v -> Either.Left $ "Unknown ContentRatingSystem: " <> v

contentRatingSystemSchema :: FC.Fleece t => FC.Schema t ContentRatingSystem
contentRatingSystemSchema =
  FC.boundedEnum contentRatingSystemToText