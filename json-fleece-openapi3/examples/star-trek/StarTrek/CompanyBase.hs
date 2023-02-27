{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CompanyBase
  ( CompanyBase(..)
  , companyBaseSchema
  ) where

import Data.Text (Text)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Bool, Eq, Maybe, Show)

data CompanyBase = CompanyBase
  { name :: Text -- ^ Company name
  , productionCompany :: Maybe Bool -- ^ Whether it's a production company
  , makeUpEffectsStudio :: Maybe Bool -- ^ Whether it's a make-up effects studio
  , recordLabel :: Maybe Bool -- ^ Whether it's a record label
  , postProductionCompany :: Maybe Bool -- ^ Whether it's a post-production company
  , conglomerate :: Maybe Bool -- ^ Whether it's a conglomerate
  , videoGameCompany :: Maybe Bool -- ^ Whether it's a video game company
  , tvAndFilmProductionCompany :: Maybe Bool -- ^ Whether it's a TV and film production company
  , broadcaster :: Maybe Bool -- ^ Whether it's a broadcaster
  , uid :: Text -- ^ Company unique ID
  , mattePaintingCompany :: Maybe Bool -- ^ Whether it's a matte painting company
  , modelAndMiniatureEffectsCompany :: Maybe Bool -- ^ Whether it's a model and miniature effects company
  , specialEffectsCompany :: Maybe Bool -- ^ Whether it's a special effects company
  , gameCompany :: Maybe Bool -- ^ Whether it's a game company
  , distributor :: Maybe Bool -- ^ Whether it's a distributor
  , propCompany :: Maybe Bool -- ^ Whether it's a prop company
  , digitalVisualEffectsCompany :: Maybe Bool -- ^ Whether it's a digital visual effects company
  , collectibleCompany :: Maybe Bool -- ^ Whether it's a collectible company
  , filmEquipmentCompany :: Maybe Bool -- ^ Whether it's a film equipment company
  }
  deriving (Eq, Show)

companyBaseSchema :: FC.Fleece schema => schema CompanyBase
companyBaseSchema =
  FC.object $
    FC.constructor CompanyBase
      #+ FC.required "name" name FC.text
      #+ FC.optional "productionCompany" productionCompany FC.boolean
      #+ FC.optional "makeUpEffectsStudio" makeUpEffectsStudio FC.boolean
      #+ FC.optional "recordLabel" recordLabel FC.boolean
      #+ FC.optional "postProductionCompany" postProductionCompany FC.boolean
      #+ FC.optional "conglomerate" conglomerate FC.boolean
      #+ FC.optional "videoGameCompany" videoGameCompany FC.boolean
      #+ FC.optional "tvAndFilmProductionCompany" tvAndFilmProductionCompany FC.boolean
      #+ FC.optional "broadcaster" broadcaster FC.boolean
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "mattePaintingCompany" mattePaintingCompany FC.boolean
      #+ FC.optional "modelAndMiniatureEffectsCompany" modelAndMiniatureEffectsCompany FC.boolean
      #+ FC.optional "specialEffectsCompany" specialEffectsCompany FC.boolean
      #+ FC.optional "gameCompany" gameCompany FC.boolean
      #+ FC.optional "distributor" distributor FC.boolean
      #+ FC.optional "propCompany" propCompany FC.boolean
      #+ FC.optional "digitalVisualEffectsCompany" digitalVisualEffectsCompany FC.boolean
      #+ FC.optional "collectibleCompany" collectibleCompany FC.boolean
      #+ FC.optional "filmEquipmentCompany" filmEquipmentCompany FC.boolean