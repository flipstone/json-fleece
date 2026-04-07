{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.AstronomicalObjectType
  ( AstronomicalObjectType(..)
  , astronomicalObjectTypeSchema
  , astronomicalObjectTypeToText
  , astronomicalObjectTypeFromText
  ) where

import qualified Data.Either as Either
import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (($), (<>), Bounded, Enum, Eq, Ord, Show, String)

data AstronomicalObjectType
  = PLANET
  | DCLASSPLANET
  | HCLASSPLANET
  | GASGIANTPLANET
  | KCLASSPLANET
  | LCLASSPLANET
  | MCLASSPLANET
  | YCLASSPLANET
  | ROGUEPLANET
  | ARTIFICIALPLANET
  | ASTEROID
  | ASTEROIDALMOON
  | ASTEROIDBELT
  | CLUSTER
  | COMET
  | CONSTELLATION
  | GALAXY
  | MOON
  | MCLASSMOON
  | NEBULA
  | PLANETOID
  | DCLASSPLANETOID
  | QUASAR
  | STAR
  | STARSYSTEM
  | SECTOR
  | REGION
  deriving (Eq, Show, Ord, Enum, Bounded)

astronomicalObjectTypeToText :: AstronomicalObjectType -> T.Text
astronomicalObjectTypeToText v =
  T.pack $
    case v of
      PLANET -> "PLANET"
      DCLASSPLANET -> "D_CLASS_PLANET"
      HCLASSPLANET -> "H_CLASS_PLANET"
      GASGIANTPLANET -> "GAS_GIANT_PLANET"
      KCLASSPLANET -> "K_CLASS_PLANET"
      LCLASSPLANET -> "L_CLASS_PLANET"
      MCLASSPLANET -> "M_CLASS_PLANET"
      YCLASSPLANET -> "Y_CLASS_PLANET"
      ROGUEPLANET -> "ROGUE_PLANET"
      ARTIFICIALPLANET -> "ARTIFICIAL_PLANET"
      ASTEROID -> "ASTEROID"
      ASTEROIDALMOON -> "ASTEROIDAL_MOON"
      ASTEROIDBELT -> "ASTEROID_BELT"
      CLUSTER -> "CLUSTER"
      COMET -> "COMET"
      CONSTELLATION -> "CONSTELLATION"
      GALAXY -> "GALAXY"
      MOON -> "MOON"
      MCLASSMOON -> "M_CLASS_MOON"
      NEBULA -> "NEBULA"
      PLANETOID -> "PLANETOID"
      DCLASSPLANETOID -> "D_CLASS_PLANETOID"
      QUASAR -> "QUASAR"
      STAR -> "STAR"
      STARSYSTEM -> "STAR_SYSTEM"
      SECTOR -> "SECTOR"
      REGION -> "REGION"

astronomicalObjectTypeFromText :: T.Text -> Either.Either String AstronomicalObjectType
astronomicalObjectTypeFromText txt =
  case T.unpack txt of
    "PLANET" -> Either.Right PLANET
    "D_CLASS_PLANET" -> Either.Right DCLASSPLANET
    "H_CLASS_PLANET" -> Either.Right HCLASSPLANET
    "GAS_GIANT_PLANET" -> Either.Right GASGIANTPLANET
    "K_CLASS_PLANET" -> Either.Right KCLASSPLANET
    "L_CLASS_PLANET" -> Either.Right LCLASSPLANET
    "M_CLASS_PLANET" -> Either.Right MCLASSPLANET
    "Y_CLASS_PLANET" -> Either.Right YCLASSPLANET
    "ROGUE_PLANET" -> Either.Right ROGUEPLANET
    "ARTIFICIAL_PLANET" -> Either.Right ARTIFICIALPLANET
    "ASTEROID" -> Either.Right ASTEROID
    "ASTEROIDAL_MOON" -> Either.Right ASTEROIDALMOON
    "ASTEROID_BELT" -> Either.Right ASTEROIDBELT
    "CLUSTER" -> Either.Right CLUSTER
    "COMET" -> Either.Right COMET
    "CONSTELLATION" -> Either.Right CONSTELLATION
    "GALAXY" -> Either.Right GALAXY
    "MOON" -> Either.Right MOON
    "M_CLASS_MOON" -> Either.Right MCLASSMOON
    "NEBULA" -> Either.Right NEBULA
    "PLANETOID" -> Either.Right PLANETOID
    "D_CLASS_PLANETOID" -> Either.Right DCLASSPLANETOID
    "QUASAR" -> Either.Right QUASAR
    "STAR" -> Either.Right STAR
    "STAR_SYSTEM" -> Either.Right STARSYSTEM
    "SECTOR" -> Either.Right SECTOR
    "REGION" -> Either.Right REGION
    v -> Either.Left $ "Unknown AstronomicalObjectType: " <> v

astronomicalObjectTypeSchema :: FC.Fleece t => FC.Schema t AstronomicalObjectType
astronomicalObjectTypeSchema =
  FC.boundedEnum astronomicalObjectTypeToText