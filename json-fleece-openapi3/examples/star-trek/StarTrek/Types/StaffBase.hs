{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffBase
  ( StaffBase(..)
  , staffBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.Gender as Gender
import qualified StarTrek.Types.StaffBase.ArtDepartment as ArtDepartment
import qualified StarTrek.Types.StaffBase.ArtDirector as ArtDirector
import qualified StarTrek.Types.StaffBase.AssistantOrSecondUnitDirector as AssistantOrSecondUnitDirector
import qualified StarTrek.Types.StaffBase.AudioAuthor as AudioAuthor
import qualified StarTrek.Types.StaffBase.Author as Author
import qualified StarTrek.Types.StaffBase.BirthName as BirthName
import qualified StarTrek.Types.StaffBase.CalendarArtist as CalendarArtist
import qualified StarTrek.Types.StaffBase.CameraAndElectricalDepartment as CameraAndElectricalDepartment
import qualified StarTrek.Types.StaffBase.CastingDepartment as CastingDepartment
import qualified StarTrek.Types.StaffBase.CbsDigitalStaff as CbsDigitalStaff
import qualified StarTrek.Types.StaffBase.Cinematographer as Cinematographer
import qualified StarTrek.Types.StaffBase.ComicArtist as ComicArtist
import qualified StarTrek.Types.StaffBase.ComicAuthor as ComicAuthor
import qualified StarTrek.Types.StaffBase.ComicColorArtist as ComicColorArtist
import qualified StarTrek.Types.StaffBase.ComicInkArtist as ComicInkArtist
import qualified StarTrek.Types.StaffBase.ComicInteriorArtist as ComicInteriorArtist
import qualified StarTrek.Types.StaffBase.ComicLetterArtist as ComicLetterArtist
import qualified StarTrek.Types.StaffBase.ComicPencilArtist as ComicPencilArtist
import qualified StarTrek.Types.StaffBase.ComicStripArtist as ComicStripArtist
import qualified StarTrek.Types.StaffBase.Composer as Composer
import qualified StarTrek.Types.StaffBase.CostumeDepartment as CostumeDepartment
import qualified StarTrek.Types.StaffBase.CostumeDesigner as CostumeDesigner
import qualified StarTrek.Types.StaffBase.DateOfBirth as DateOfBirth
import qualified StarTrek.Types.StaffBase.DateOfDeath as DateOfDeath
import qualified StarTrek.Types.StaffBase.Director as Director
import qualified StarTrek.Types.StaffBase.ExhibitAndAttractionStaff as ExhibitAndAttractionStaff
import qualified StarTrek.Types.StaffBase.FilmEditor as FilmEditor
import qualified StarTrek.Types.StaffBase.GameArtist as GameArtist
import qualified StarTrek.Types.StaffBase.GameAuthor as GameAuthor
import qualified StarTrek.Types.StaffBase.IlmProductionStaff as IlmProductionStaff
import qualified StarTrek.Types.StaffBase.Linguist as Linguist
import qualified StarTrek.Types.StaffBase.LocationStaff as LocationStaff
import qualified StarTrek.Types.StaffBase.MakeupStaff as MakeupStaff
import qualified StarTrek.Types.StaffBase.MusicDepartment as MusicDepartment
import qualified StarTrek.Types.StaffBase.Name as Name
import qualified StarTrek.Types.StaffBase.NovelArtist as NovelArtist
import qualified StarTrek.Types.StaffBase.NovelAuthor as NovelAuthor
import qualified StarTrek.Types.StaffBase.PersonalAssistant as PersonalAssistant
import qualified StarTrek.Types.StaffBase.PlaceOfBirth as PlaceOfBirth
import qualified StarTrek.Types.StaffBase.PlaceOfDeath as PlaceOfDeath
import qualified StarTrek.Types.StaffBase.Producer as Producer
import qualified StarTrek.Types.StaffBase.ProductionAssociate as ProductionAssociate
import qualified StarTrek.Types.StaffBase.ProductionDesigner as ProductionDesigner
import qualified StarTrek.Types.StaffBase.ProductionStaff as ProductionStaff
import qualified StarTrek.Types.StaffBase.PublicationArtist as PublicationArtist
import qualified StarTrek.Types.StaffBase.PublicationDesigner as PublicationDesigner
import qualified StarTrek.Types.StaffBase.PublicationEditor as PublicationEditor
import qualified StarTrek.Types.StaffBase.PublicationStaff as PublicationStaff
import qualified StarTrek.Types.StaffBase.PublicityArtist as PublicityArtist
import qualified StarTrek.Types.StaffBase.ReferenceArtist as ReferenceArtist
import qualified StarTrek.Types.StaffBase.ReferenceAuthor as ReferenceAuthor
import qualified StarTrek.Types.StaffBase.ScienceConsultant as ScienceConsultant
import qualified StarTrek.Types.StaffBase.SoundDepartment as SoundDepartment
import qualified StarTrek.Types.StaffBase.SpecialAndVisualEffectsStaff as SpecialAndVisualEffectsStaff
import qualified StarTrek.Types.StaffBase.SpecialFeaturesStaff as SpecialFeaturesStaff
import qualified StarTrek.Types.StaffBase.StoryEditor as StoryEditor
import qualified StarTrek.Types.StaffBase.StudioExecutive as StudioExecutive
import qualified StarTrek.Types.StaffBase.StuntDepartment as StuntDepartment
import qualified StarTrek.Types.StaffBase.TransportationDepartment as TransportationDepartment
import qualified StarTrek.Types.StaffBase.Uid as Uid
import qualified StarTrek.Types.StaffBase.VideoGameProductionStaff as VideoGameProductionStaff
import qualified StarTrek.Types.StaffBase.Writer as Writer

data StaffBase = StaffBase
  { studioExecutive :: Maybe StudioExecutive.StudioExecutive -- ^ Whether this person is a studio executive
  , birthName :: Maybe BirthName.BirthName -- ^ Staff birth name
  , gameArtist :: Maybe GameArtist.GameArtist -- ^ Whether this person is a game artist
  , filmEditor :: Maybe FilmEditor.FilmEditor -- ^ Whether this person is a film editor
  , comicStripArtist :: Maybe ComicStripArtist.ComicStripArtist -- ^ Whether this person is a comic strip artist
  , castingDepartment :: Maybe CastingDepartment.CastingDepartment -- ^ Whether this person is from casting department
  , costumeDesigner :: Maybe CostumeDesigner.CostumeDesigner -- ^ Whether this person is a custume designer
  , author :: Maybe Author.Author -- ^ Whether this person is an author
  , name :: Name.Name -- ^ Staff name
  , personalAssistant :: Maybe PersonalAssistant.PersonalAssistant -- ^ Whether this person is a personal assistant
  , assistantOrSecondUnitDirector :: Maybe AssistantOrSecondUnitDirector.AssistantOrSecondUnitDirector -- ^ Whether this person is an assistant or secound unit director director
  , gameAuthor :: Maybe GameAuthor.GameAuthor -- ^ Whether this person is a game author
  , publicityArtist :: Maybe PublicityArtist.PublicityArtist -- ^ Whether this person is a publication artist
  , publicationStaff :: Maybe PublicationStaff.PublicationStaff -- ^ Whether this person is a publication staff
  , soundDepartment :: Maybe SoundDepartment.SoundDepartment -- ^ Whether this person is from sound department
  , composer :: Maybe Composer.Composer -- ^ Whether this person is a composer
  , publicationArtist :: Maybe PublicationArtist.PublicationArtist -- ^ Whether this person is a publication artist
  , referenceArtist :: Maybe ReferenceArtist.ReferenceArtist -- ^ Whether this person is a reference artist
  , comicColorArtist :: Maybe ComicColorArtist.ComicColorArtist -- ^ Whether this person is a comic color artist
  , comicAuthor :: Maybe ComicAuthor.ComicAuthor -- ^ Whether this person is a comic author
  , director :: Maybe Director.Director -- ^ Whether this person is a director
  , artDepartment :: Maybe ArtDepartment.ArtDepartment -- ^ Whether this person if from art department
  , publicationDesigner :: Maybe PublicationDesigner.PublicationDesigner -- ^ Whether this person is a publication designer
  , comicInkArtist :: Maybe ComicInkArtist.ComicInkArtist -- ^ Whether this person is a comic ink artist
  , comicArtist :: Maybe ComicArtist.ComicArtist -- ^ Whether this person is a comic artist
  , referenceAuthor :: Maybe ReferenceAuthor.ReferenceAuthor -- ^ Whether this person is a reference author
  , cameraAndElectricalDepartment :: Maybe CameraAndElectricalDepartment.CameraAndElectricalDepartment -- ^ Whether this person is from camera and electrical department
  , exhibitAndAttractionStaff :: Maybe ExhibitAndAttractionStaff.ExhibitAndAttractionStaff -- ^ Whether this person is an exhibit and tttraction staff
  , stuntDepartment :: Maybe StuntDepartment.StuntDepartment -- ^ Whether this person is from stunt department
  , uid :: Uid.Uid -- ^ Staff unique ID
  , writer :: Maybe Writer.Writer -- ^ Whether this person is a writer
  , audioAuthor :: Maybe AudioAuthor.AudioAuthor -- ^ Whether this person is an audio author
  , transportationDepartment :: Maybe TransportationDepartment.TransportationDepartment -- ^ Whether this person is from transportation department
  , linguist :: Maybe Linguist.Linguist -- ^ Whether this person is a linguist
  , cinematographer :: Maybe Cinematographer.Cinematographer -- ^ Whether this person is a cinematographer
  , dateOfDeath :: Maybe DateOfDeath.DateOfDeath -- ^ Date the staff died
  , locationStaff :: Maybe LocationStaff.LocationStaff -- ^ Whether this person is a location staff
  , calendarArtist :: Maybe CalendarArtist.CalendarArtist -- ^ Whether this person is a calendar artist
  , costumeDepartment :: Maybe CostumeDepartment.CostumeDepartment -- ^ Whether this person is from costume department
  , novelArtist :: Maybe NovelArtist.NovelArtist -- ^ Whether this person is a novel artist
  , artDirector :: Maybe ArtDirector.ArtDirector -- ^ Whether this person is an art director
  , productionAssociate :: Maybe ProductionAssociate.ProductionAssociate -- ^ Whether this person is a production associate
  , cbsDigitalStaff :: Maybe CbsDigitalStaff.CbsDigitalStaff -- ^ Whether this person is a part of CBS digital staff
  , storyEditor :: Maybe StoryEditor.StoryEditor -- ^ Whether this person is a story editor
  , comicInteriorArtist :: Maybe ComicInteriorArtist.ComicInteriorArtist -- ^ Whether this person is a comic interior artist
  , specialFeaturesStaff :: Maybe SpecialFeaturesStaff.SpecialFeaturesStaff -- ^ Whether this person is a special features artist
  , scienceConsultant :: Maybe ScienceConsultant.ScienceConsultant -- ^ Whether this person is a science consultant
  , gender :: Maybe Gender.Gender -- ^ Gender
  , dateOfBirth :: Maybe DateOfBirth.DateOfBirth -- ^ Date the staff was born
  , publicationEditor :: Maybe PublicationEditor.PublicationEditor -- ^ Whether this person is a publication editor
  , placeOfDeath :: Maybe PlaceOfDeath.PlaceOfDeath -- ^ Place the staff died
  , novelAuthor :: Maybe NovelAuthor.NovelAuthor -- ^ Whether this person is a novel author
  , ilmProductionStaff :: Maybe IlmProductionStaff.IlmProductionStaff -- ^ Whether this person is a part of ILM production staff
  , producer :: Maybe Producer.Producer -- ^ Whether this person is a producer
  , musicDepartment :: Maybe MusicDepartment.MusicDepartment -- ^ Whether this person is from music department
  , comicPencilArtist :: Maybe ComicPencilArtist.ComicPencilArtist -- ^ Whether this person is a comic pencil artist
  , specialAndVisualEffectsStaff :: Maybe SpecialAndVisualEffectsStaff.SpecialAndVisualEffectsStaff -- ^ Whether this person is a special and visual effects staff
  , makeupStaff :: Maybe MakeupStaff.MakeupStaff -- ^ Whether this person is a make-up staff
  , comicLetterArtist :: Maybe ComicLetterArtist.ComicLetterArtist -- ^ Whether this person is a comic letter artist
  , placeOfBirth :: Maybe PlaceOfBirth.PlaceOfBirth -- ^ Place the staff was born
  , productionDesigner :: Maybe ProductionDesigner.ProductionDesigner -- ^ Whether this person is a production designer
  , videoGameProductionStaff :: Maybe VideoGameProductionStaff.VideoGameProductionStaff -- ^ Whether this person is video game production staff
  , productionStaff :: Maybe ProductionStaff.ProductionStaff -- ^ Whether this person is a production staff
  }
  deriving (Eq, Show)

staffBaseSchema :: FC.Fleece schema => schema StaffBase
staffBaseSchema =
  FC.object $
    FC.constructor StaffBase
      #+ FC.optional "studioExecutive" studioExecutive StudioExecutive.studioExecutiveSchema
      #+ FC.optional "birthName" birthName BirthName.birthNameSchema
      #+ FC.optional "gameArtist" gameArtist GameArtist.gameArtistSchema
      #+ FC.optional "filmEditor" filmEditor FilmEditor.filmEditorSchema
      #+ FC.optional "comicStripArtist" comicStripArtist ComicStripArtist.comicStripArtistSchema
      #+ FC.optional "castingDepartment" castingDepartment CastingDepartment.castingDepartmentSchema
      #+ FC.optional "costumeDesigner" costumeDesigner CostumeDesigner.costumeDesignerSchema
      #+ FC.optional "author" author Author.authorSchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "personalAssistant" personalAssistant PersonalAssistant.personalAssistantSchema
      #+ FC.optional "assistantOrSecondUnitDirector" assistantOrSecondUnitDirector AssistantOrSecondUnitDirector.assistantOrSecondUnitDirectorSchema
      #+ FC.optional "gameAuthor" gameAuthor GameAuthor.gameAuthorSchema
      #+ FC.optional "publicityArtist" publicityArtist PublicityArtist.publicityArtistSchema
      #+ FC.optional "publicationStaff" publicationStaff PublicationStaff.publicationStaffSchema
      #+ FC.optional "soundDepartment" soundDepartment SoundDepartment.soundDepartmentSchema
      #+ FC.optional "composer" composer Composer.composerSchema
      #+ FC.optional "publicationArtist" publicationArtist PublicationArtist.publicationArtistSchema
      #+ FC.optional "referenceArtist" referenceArtist ReferenceArtist.referenceArtistSchema
      #+ FC.optional "comicColorArtist" comicColorArtist ComicColorArtist.comicColorArtistSchema
      #+ FC.optional "comicAuthor" comicAuthor ComicAuthor.comicAuthorSchema
      #+ FC.optional "director" director Director.directorSchema
      #+ FC.optional "artDepartment" artDepartment ArtDepartment.artDepartmentSchema
      #+ FC.optional "publicationDesigner" publicationDesigner PublicationDesigner.publicationDesignerSchema
      #+ FC.optional "comicInkArtist" comicInkArtist ComicInkArtist.comicInkArtistSchema
      #+ FC.optional "comicArtist" comicArtist ComicArtist.comicArtistSchema
      #+ FC.optional "referenceAuthor" referenceAuthor ReferenceAuthor.referenceAuthorSchema
      #+ FC.optional "cameraAndElectricalDepartment" cameraAndElectricalDepartment CameraAndElectricalDepartment.cameraAndElectricalDepartmentSchema
      #+ FC.optional "exhibitAndAttractionStaff" exhibitAndAttractionStaff ExhibitAndAttractionStaff.exhibitAndAttractionStaffSchema
      #+ FC.optional "stuntDepartment" stuntDepartment StuntDepartment.stuntDepartmentSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "writer" writer Writer.writerSchema
      #+ FC.optional "audioAuthor" audioAuthor AudioAuthor.audioAuthorSchema
      #+ FC.optional "transportationDepartment" transportationDepartment TransportationDepartment.transportationDepartmentSchema
      #+ FC.optional "linguist" linguist Linguist.linguistSchema
      #+ FC.optional "cinematographer" cinematographer Cinematographer.cinematographerSchema
      #+ FC.optional "dateOfDeath" dateOfDeath DateOfDeath.dateOfDeathSchema
      #+ FC.optional "locationStaff" locationStaff LocationStaff.locationStaffSchema
      #+ FC.optional "calendarArtist" calendarArtist CalendarArtist.calendarArtistSchema
      #+ FC.optional "costumeDepartment" costumeDepartment CostumeDepartment.costumeDepartmentSchema
      #+ FC.optional "novelArtist" novelArtist NovelArtist.novelArtistSchema
      #+ FC.optional "artDirector" artDirector ArtDirector.artDirectorSchema
      #+ FC.optional "productionAssociate" productionAssociate ProductionAssociate.productionAssociateSchema
      #+ FC.optional "cbsDigitalStaff" cbsDigitalStaff CbsDigitalStaff.cbsDigitalStaffSchema
      #+ FC.optional "storyEditor" storyEditor StoryEditor.storyEditorSchema
      #+ FC.optional "comicInteriorArtist" comicInteriorArtist ComicInteriorArtist.comicInteriorArtistSchema
      #+ FC.optional "specialFeaturesStaff" specialFeaturesStaff SpecialFeaturesStaff.specialFeaturesStaffSchema
      #+ FC.optional "scienceConsultant" scienceConsultant ScienceConsultant.scienceConsultantSchema
      #+ FC.optional "gender" gender Gender.genderSchema
      #+ FC.optional "dateOfBirth" dateOfBirth DateOfBirth.dateOfBirthSchema
      #+ FC.optional "publicationEditor" publicationEditor PublicationEditor.publicationEditorSchema
      #+ FC.optional "placeOfDeath" placeOfDeath PlaceOfDeath.placeOfDeathSchema
      #+ FC.optional "novelAuthor" novelAuthor NovelAuthor.novelAuthorSchema
      #+ FC.optional "ilmProductionStaff" ilmProductionStaff IlmProductionStaff.ilmProductionStaffSchema
      #+ FC.optional "producer" producer Producer.producerSchema
      #+ FC.optional "musicDepartment" musicDepartment MusicDepartment.musicDepartmentSchema
      #+ FC.optional "comicPencilArtist" comicPencilArtist ComicPencilArtist.comicPencilArtistSchema
      #+ FC.optional "specialAndVisualEffectsStaff" specialAndVisualEffectsStaff SpecialAndVisualEffectsStaff.specialAndVisualEffectsStaffSchema
      #+ FC.optional "makeupStaff" makeupStaff MakeupStaff.makeupStaffSchema
      #+ FC.optional "comicLetterArtist" comicLetterArtist ComicLetterArtist.comicLetterArtistSchema
      #+ FC.optional "placeOfBirth" placeOfBirth PlaceOfBirth.placeOfBirthSchema
      #+ FC.optional "productionDesigner" productionDesigner ProductionDesigner.productionDesignerSchema
      #+ FC.optional "videoGameProductionStaff" videoGameProductionStaff VideoGameProductionStaff.videoGameProductionStaffSchema
      #+ FC.optional "productionStaff" productionStaff ProductionStaff.productionStaffSchema