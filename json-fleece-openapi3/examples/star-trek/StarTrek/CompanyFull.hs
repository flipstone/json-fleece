{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CompanyFull
  ( CompanyFull(..)
  , companyFullSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Bool, Eq, Maybe, Show)

data CompanyFull = CompanyFull
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

companyFullSchema :: FC.Fleece schema => schema CompanyFull
companyFullSchema =
  FC.object $
    FC.constructor CompanyFull
      #+ FC.required "name" name FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "productionCompany" productionCompany FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "makeUpEffectsStudio" makeUpEffectsStudio FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "recordLabel" recordLabel FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "postProductionCompany" postProductionCompany FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "conglomerate" conglomerate FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "videoGameCompany" videoGameCompany FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "tvAndFilmProductionCompany" tvAndFilmProductionCompany FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "broadcaster" broadcaster FC.boolean
      #+ FC.required "uid" uid FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "mattePaintingCompany" mattePaintingCompany FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "modelAndMiniatureEffectsCompany" modelAndMiniatureEffectsCompany FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "specialEffectsCompany" specialEffectsCompany FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "gameCompany" gameCompany FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "distributor" distributor FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "propCompany" propCompany FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "digitalVisualEffectsCompany" digitalVisualEffectsCompany FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "collectibleCompany" collectibleCompany FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "filmEquipmentCompany" filmEquipmentCompany FC.boolean