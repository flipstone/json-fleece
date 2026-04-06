{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.VideoReleaseFormat
  ( VideoReleaseFormat(..)
  , videoReleaseFormatSchema
  , videoReleaseFormatToText
  , videoReleaseFormatFromText
  ) where

import qualified Data.Either as Either
import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (($), (<>), Bounded, Either, Enum, Eq, Ord, Show, String)

data VideoReleaseFormat
  = SUPER8
  | BETAMAX
  | VHS
  | CED
  | LD
  | VHD
  | VCD
  | VIDEO8
  | DVD
  | UMD
  | HDDVD
  | BLURAY
  | BLURAY4KUHD
  | DIGITALFORMAT
  deriving (Eq, Show, Ord, Enum, Bounded)

videoReleaseFormatToText :: VideoReleaseFormat -> T.Text
videoReleaseFormatToText v =
  T.pack $
    case v of
      SUPER8 -> "SUPER_8"
      BETAMAX -> "BETAMAX"
      VHS -> "VHS"
      CED -> "CED"
      LD -> "LD"
      VHD -> "VHD"
      VCD -> "VCD"
      VIDEO8 -> "VIDEO_8"
      DVD -> "DVD"
      UMD -> "UMD"
      HDDVD -> "HD_DVD"
      BLURAY -> "BLU_RAY"
      BLURAY4KUHD -> "BLU_RAY_4K_UHD"
      DIGITALFORMAT -> "DIGITAL_FORMAT"

videoReleaseFormatFromText :: T.Text -> Either String VideoReleaseFormat
videoReleaseFormatFromText txt =
  case T.unpack txt of
    "SUPER_8" -> Either.Right SUPER8
    "BETAMAX" -> Either.Right BETAMAX
    "VHS" -> Either.Right VHS
    "CED" -> Either.Right CED
    "LD" -> Either.Right LD
    "VHD" -> Either.Right VHD
    "VCD" -> Either.Right VCD
    "VIDEO_8" -> Either.Right VIDEO8
    "DVD" -> Either.Right DVD
    "UMD" -> Either.Right UMD
    "HD_DVD" -> Either.Right HDDVD
    "BLU_RAY" -> Either.Right BLURAY
    "BLU_RAY_4K_UHD" -> Either.Right BLURAY4KUHD
    "DIGITAL_FORMAT" -> Either.Right DIGITALFORMAT
    v -> Either.Left $ "Unknown VideoReleaseFormat: " <> v

videoReleaseFormatSchema :: FC.Fleece t => FC.Schema t VideoReleaseFormat
videoReleaseFormatSchema =
  FC.boundedEnum videoReleaseFormatToText