{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CompanyBase
  ( CompanyBase(..)
  , companyBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.CompanyBase.Broadcaster (Broadcaster, broadcasterSchema)
import StarTrek.CompanyBase.CollectibleCompany (CollectibleCompany, collectibleCompanySchema)
import StarTrek.CompanyBase.Conglomerate (Conglomerate, conglomerateSchema)
import StarTrek.CompanyBase.DigitalVisualEffectsCompany (DigitalVisualEffectsCompany, digitalVisualEffectsCompanySchema)
import StarTrek.CompanyBase.Distributor (Distributor, distributorSchema)
import StarTrek.CompanyBase.FilmEquipmentCompany (FilmEquipmentCompany, filmEquipmentCompanySchema)
import StarTrek.CompanyBase.GameCompany (GameCompany, gameCompanySchema)
import StarTrek.CompanyBase.MakeUpEffectsStudio (MakeUpEffectsStudio, makeUpEffectsStudioSchema)
import StarTrek.CompanyBase.MattePaintingCompany (MattePaintingCompany, mattePaintingCompanySchema)
import StarTrek.CompanyBase.ModelAndMiniatureEffectsCompany (ModelAndMiniatureEffectsCompany, modelAndMiniatureEffectsCompanySchema)
import StarTrek.CompanyBase.Name (Name, nameSchema)
import StarTrek.CompanyBase.PostProductionCompany (PostProductionCompany, postProductionCompanySchema)
import StarTrek.CompanyBase.ProductionCompany (ProductionCompany, productionCompanySchema)
import StarTrek.CompanyBase.PropCompany (PropCompany, propCompanySchema)
import StarTrek.CompanyBase.RecordLabel (RecordLabel, recordLabelSchema)
import StarTrek.CompanyBase.SpecialEffectsCompany (SpecialEffectsCompany, specialEffectsCompanySchema)
import StarTrek.CompanyBase.TvAndFilmProductionCompany (TvAndFilmProductionCompany, tvAndFilmProductionCompanySchema)
import StarTrek.CompanyBase.Uid (Uid, uidSchema)
import StarTrek.CompanyBase.VideoGameCompany (VideoGameCompany, videoGameCompanySchema)

data CompanyBase = CompanyBase
  { name :: Name -- ^ Company name
  , productionCompany :: Maybe ProductionCompany -- ^ Whether it's a production company
  , makeUpEffectsStudio :: Maybe MakeUpEffectsStudio -- ^ Whether it's a make-up effects studio
  , recordLabel :: Maybe RecordLabel -- ^ Whether it's a record label
  , postProductionCompany :: Maybe PostProductionCompany -- ^ Whether it's a post-production company
  , conglomerate :: Maybe Conglomerate -- ^ Whether it's a conglomerate
  , videoGameCompany :: Maybe VideoGameCompany -- ^ Whether it's a video game company
  , tvAndFilmProductionCompany :: Maybe TvAndFilmProductionCompany -- ^ Whether it's a TV and film production company
  , broadcaster :: Maybe Broadcaster -- ^ Whether it's a broadcaster
  , uid :: Uid -- ^ Company unique ID
  , mattePaintingCompany :: Maybe MattePaintingCompany -- ^ Whether it's a matte painting company
  , modelAndMiniatureEffectsCompany :: Maybe ModelAndMiniatureEffectsCompany -- ^ Whether it's a model and miniature effects company
  , specialEffectsCompany :: Maybe SpecialEffectsCompany -- ^ Whether it's a special effects company
  , gameCompany :: Maybe GameCompany -- ^ Whether it's a game company
  , distributor :: Maybe Distributor -- ^ Whether it's a distributor
  , propCompany :: Maybe PropCompany -- ^ Whether it's a prop company
  , digitalVisualEffectsCompany :: Maybe DigitalVisualEffectsCompany -- ^ Whether it's a digital visual effects company
  , collectibleCompany :: Maybe CollectibleCompany -- ^ Whether it's a collectible company
  , filmEquipmentCompany :: Maybe FilmEquipmentCompany -- ^ Whether it's a film equipment company
  }
  deriving (Eq, Show)

companyBaseSchema :: FC.Fleece schema => schema CompanyBase
companyBaseSchema =
  FC.object $
    FC.constructor CompanyBase
      #+ FC.required "name" name nameSchema
      #+ FC.optional "productionCompany" productionCompany productionCompanySchema
      #+ FC.optional "makeUpEffectsStudio" makeUpEffectsStudio makeUpEffectsStudioSchema
      #+ FC.optional "recordLabel" recordLabel recordLabelSchema
      #+ FC.optional "postProductionCompany" postProductionCompany postProductionCompanySchema
      #+ FC.optional "conglomerate" conglomerate conglomerateSchema
      #+ FC.optional "videoGameCompany" videoGameCompany videoGameCompanySchema
      #+ FC.optional "tvAndFilmProductionCompany" tvAndFilmProductionCompany tvAndFilmProductionCompanySchema
      #+ FC.optional "broadcaster" broadcaster broadcasterSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "mattePaintingCompany" mattePaintingCompany mattePaintingCompanySchema
      #+ FC.optional "modelAndMiniatureEffectsCompany" modelAndMiniatureEffectsCompany modelAndMiniatureEffectsCompanySchema
      #+ FC.optional "specialEffectsCompany" specialEffectsCompany specialEffectsCompanySchema
      #+ FC.optional "gameCompany" gameCompany gameCompanySchema
      #+ FC.optional "distributor" distributor distributorSchema
      #+ FC.optional "propCompany" propCompany propCompanySchema
      #+ FC.optional "digitalVisualEffectsCompany" digitalVisualEffectsCompany digitalVisualEffectsCompanySchema
      #+ FC.optional "collectibleCompany" collectibleCompany collectibleCompanySchema
      #+ FC.optional "filmEquipmentCompany" filmEquipmentCompany filmEquipmentCompanySchema