{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ContentRatingSystem
  ( ContentRatingSystem(..)
  , contentRatingSystemSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (($), Bounded, Enum, Eq, Ord, Show)

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

contentRatingSystemSchema :: FC.Fleece schema => schema ContentRatingSystem
contentRatingSystemSchema =
  FC.boundedEnum contentRatingSystemToText