{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.AstronomicalObjectType
  ( AstronomicalObjectType(..)
  , astronomicalObjectTypeSchema
  , astronomicalObjectTypeToText
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (($), Bounded, Enum, Eq, Ord, Show)

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

astronomicalObjectTypeSchema :: FC.Fleece schema => schema AstronomicalObjectType
astronomicalObjectTypeSchema =
  FC.boundedEnum astronomicalObjectTypeToText