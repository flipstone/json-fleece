{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoReleaseFormat
  ( VideoReleaseFormat(..)
  , videoReleaseFormatSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text, pack)
import Prelude (($), Bounded, Enum, Eq, Ord, Show)

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

videoReleaseFormatToText :: VideoReleaseFormat -> Text
videoReleaseFormatToText v =
  pack $
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

videoReleaseFormatSchema :: FC.Fleece schema => schema VideoReleaseFormat
videoReleaseFormatSchema =
  FC.boundedEnum videoReleaseFormatToText