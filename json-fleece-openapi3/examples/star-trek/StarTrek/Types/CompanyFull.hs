{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.CompanyFull
  ( CompanyFull(..)
  , companyFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.CompanyFull.Broadcaster as Broadcaster
import qualified StarTrek.Types.CompanyFull.CollectibleCompany as CollectibleCompany
import qualified StarTrek.Types.CompanyFull.Conglomerate as Conglomerate
import qualified StarTrek.Types.CompanyFull.DigitalVisualEffectsCompany as DigitalVisualEffectsCompany
import qualified StarTrek.Types.CompanyFull.Distributor as Distributor
import qualified StarTrek.Types.CompanyFull.FilmEquipmentCompany as FilmEquipmentCompany
import qualified StarTrek.Types.CompanyFull.GameCompany as GameCompany
import qualified StarTrek.Types.CompanyFull.MakeUpEffectsStudio as MakeUpEffectsStudio
import qualified StarTrek.Types.CompanyFull.MattePaintingCompany as MattePaintingCompany
import qualified StarTrek.Types.CompanyFull.ModelAndMiniatureEffectsCompany as ModelAndMiniatureEffectsCompany
import qualified StarTrek.Types.CompanyFull.Name as Name
import qualified StarTrek.Types.CompanyFull.PostProductionCompany as PostProductionCompany
import qualified StarTrek.Types.CompanyFull.ProductionCompany as ProductionCompany
import qualified StarTrek.Types.CompanyFull.PropCompany as PropCompany
import qualified StarTrek.Types.CompanyFull.RecordLabel as RecordLabel
import qualified StarTrek.Types.CompanyFull.SpecialEffectsCompany as SpecialEffectsCompany
import qualified StarTrek.Types.CompanyFull.TvAndFilmProductionCompany as TvAndFilmProductionCompany
import qualified StarTrek.Types.CompanyFull.Uid as Uid
import qualified StarTrek.Types.CompanyFull.VideoGameCompany as VideoGameCompany

data CompanyFull = CompanyFull
  { makeUpEffectsStudio :: Maybe MakeUpEffectsStudio.MakeUpEffectsStudio -- ^ Whether it's a make-up effects studio
  , mattePaintingCompany :: Maybe MattePaintingCompany.MattePaintingCompany -- ^ Whether it's a matte painting company
  , specialEffectsCompany :: Maybe SpecialEffectsCompany.SpecialEffectsCompany -- ^ Whether it's a special effects company
  , productionCompany :: Maybe ProductionCompany.ProductionCompany -- ^ Whether it's a production company
  , tvAndFilmProductionCompany :: Maybe TvAndFilmProductionCompany.TvAndFilmProductionCompany -- ^ Whether it's a TV and film production company
  , videoGameCompany :: Maybe VideoGameCompany.VideoGameCompany -- ^ Whether it's a video game company
  , recordLabel :: Maybe RecordLabel.RecordLabel -- ^ Whether it's a record label
  , propCompany :: Maybe PropCompany.PropCompany -- ^ Whether it's a prop company
  , uid :: Uid.Uid -- ^ Company unique ID
  , collectibleCompany :: Maybe CollectibleCompany.CollectibleCompany -- ^ Whether it's a collectible company
  , postProductionCompany :: Maybe PostProductionCompany.PostProductionCompany -- ^ Whether it's a post-production company
  , distributor :: Maybe Distributor.Distributor -- ^ Whether it's a distributor
  , gameCompany :: Maybe GameCompany.GameCompany -- ^ Whether it's a game company
  , digitalVisualEffectsCompany :: Maybe DigitalVisualEffectsCompany.DigitalVisualEffectsCompany -- ^ Whether it's a digital visual effects company
  , name :: Name.Name -- ^ Company name
  , conglomerate :: Maybe Conglomerate.Conglomerate -- ^ Whether it's a conglomerate
  , broadcaster :: Maybe Broadcaster.Broadcaster -- ^ Whether it's a broadcaster
  , filmEquipmentCompany :: Maybe FilmEquipmentCompany.FilmEquipmentCompany -- ^ Whether it's a film equipment company
  , modelAndMiniatureEffectsCompany :: Maybe ModelAndMiniatureEffectsCompany.ModelAndMiniatureEffectsCompany -- ^ Whether it's a model and miniature effects company
  }
  deriving (Eq, Show)

companyFullSchema :: FC.Fleece schema => schema CompanyFull
companyFullSchema =
  FC.object $
    FC.constructor CompanyFull
      #+ FC.optional "makeUpEffectsStudio" makeUpEffectsStudio MakeUpEffectsStudio.makeUpEffectsStudioSchema
      #+ FC.optional "mattePaintingCompany" mattePaintingCompany MattePaintingCompany.mattePaintingCompanySchema
      #+ FC.optional "specialEffectsCompany" specialEffectsCompany SpecialEffectsCompany.specialEffectsCompanySchema
      #+ FC.optional "productionCompany" productionCompany ProductionCompany.productionCompanySchema
      #+ FC.optional "tvAndFilmProductionCompany" tvAndFilmProductionCompany TvAndFilmProductionCompany.tvAndFilmProductionCompanySchema
      #+ FC.optional "videoGameCompany" videoGameCompany VideoGameCompany.videoGameCompanySchema
      #+ FC.optional "recordLabel" recordLabel RecordLabel.recordLabelSchema
      #+ FC.optional "propCompany" propCompany PropCompany.propCompanySchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "collectibleCompany" collectibleCompany CollectibleCompany.collectibleCompanySchema
      #+ FC.optional "postProductionCompany" postProductionCompany PostProductionCompany.postProductionCompanySchema
      #+ FC.optional "distributor" distributor Distributor.distributorSchema
      #+ FC.optional "gameCompany" gameCompany GameCompany.gameCompanySchema
      #+ FC.optional "digitalVisualEffectsCompany" digitalVisualEffectsCompany DigitalVisualEffectsCompany.digitalVisualEffectsCompanySchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "conglomerate" conglomerate Conglomerate.conglomerateSchema
      #+ FC.optional "broadcaster" broadcaster Broadcaster.broadcasterSchema
      #+ FC.optional "filmEquipmentCompany" filmEquipmentCompany FilmEquipmentCompany.filmEquipmentCompanySchema
      #+ FC.optional "modelAndMiniatureEffectsCompany" modelAndMiniatureEffectsCompany ModelAndMiniatureEffectsCompany.modelAndMiniatureEffectsCompanySchema