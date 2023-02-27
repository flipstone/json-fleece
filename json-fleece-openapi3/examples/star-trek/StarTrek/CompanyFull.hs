{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CompanyFull
  ( CompanyFull(..)
  , companyFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.CompanyFull.Broadcaster (Broadcaster, broadcasterSchema)
import StarTrek.CompanyFull.CollectibleCompany (CollectibleCompany, collectibleCompanySchema)
import StarTrek.CompanyFull.Conglomerate (Conglomerate, conglomerateSchema)
import StarTrek.CompanyFull.DigitalVisualEffectsCompany (DigitalVisualEffectsCompany, digitalVisualEffectsCompanySchema)
import StarTrek.CompanyFull.Distributor (Distributor, distributorSchema)
import StarTrek.CompanyFull.FilmEquipmentCompany (FilmEquipmentCompany, filmEquipmentCompanySchema)
import StarTrek.CompanyFull.GameCompany (GameCompany, gameCompanySchema)
import StarTrek.CompanyFull.MakeUpEffectsStudio (MakeUpEffectsStudio, makeUpEffectsStudioSchema)
import StarTrek.CompanyFull.MattePaintingCompany (MattePaintingCompany, mattePaintingCompanySchema)
import StarTrek.CompanyFull.ModelAndMiniatureEffectsCompany (ModelAndMiniatureEffectsCompany, modelAndMiniatureEffectsCompanySchema)
import StarTrek.CompanyFull.Name (Name, nameSchema)
import StarTrek.CompanyFull.PostProductionCompany (PostProductionCompany, postProductionCompanySchema)
import StarTrek.CompanyFull.ProductionCompany (ProductionCompany, productionCompanySchema)
import StarTrek.CompanyFull.PropCompany (PropCompany, propCompanySchema)
import StarTrek.CompanyFull.RecordLabel (RecordLabel, recordLabelSchema)
import StarTrek.CompanyFull.SpecialEffectsCompany (SpecialEffectsCompany, specialEffectsCompanySchema)
import StarTrek.CompanyFull.TvAndFilmProductionCompany (TvAndFilmProductionCompany, tvAndFilmProductionCompanySchema)
import StarTrek.CompanyFull.Uid (Uid, uidSchema)
import StarTrek.CompanyFull.VideoGameCompany (VideoGameCompany, videoGameCompanySchema)

data CompanyFull = CompanyFull
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

companyFullSchema :: FC.Fleece schema => schema CompanyFull
companyFullSchema =
  FC.object $
    FC.constructor CompanyFull
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