{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffFull
  ( StaffFull(..)
  , staffFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.EpisodeBase as EpisodeBase
import qualified StarTrek.Types.Gender as Gender
import qualified StarTrek.Types.MovieBase as MovieBase
import qualified StarTrek.Types.StaffFull.ArtDepartment as ArtDepartment
import qualified StarTrek.Types.StaffFull.ArtDirector as ArtDirector
import qualified StarTrek.Types.StaffFull.AssistantOrSecondUnitDirector as AssistantOrSecondUnitDirector
import qualified StarTrek.Types.StaffFull.AudioAuthor as AudioAuthor
import qualified StarTrek.Types.StaffFull.Author as Author
import qualified StarTrek.Types.StaffFull.BirthName as BirthName
import qualified StarTrek.Types.StaffFull.CalendarArtist as CalendarArtist
import qualified StarTrek.Types.StaffFull.CameraAndElectricalDepartment as CameraAndElectricalDepartment
import qualified StarTrek.Types.StaffFull.CastingDepartment as CastingDepartment
import qualified StarTrek.Types.StaffFull.CbsDigitalStaff as CbsDigitalStaff
import qualified StarTrek.Types.StaffFull.Cinematographer as Cinematographer
import qualified StarTrek.Types.StaffFull.ComicArtist as ComicArtist
import qualified StarTrek.Types.StaffFull.ComicAuthor as ComicAuthor
import qualified StarTrek.Types.StaffFull.ComicColorArtist as ComicColorArtist
import qualified StarTrek.Types.StaffFull.ComicInkArtist as ComicInkArtist
import qualified StarTrek.Types.StaffFull.ComicInteriorArtist as ComicInteriorArtist
import qualified StarTrek.Types.StaffFull.ComicLetterArtist as ComicLetterArtist
import qualified StarTrek.Types.StaffFull.ComicPencilArtist as ComicPencilArtist
import qualified StarTrek.Types.StaffFull.ComicStripArtist as ComicStripArtist
import qualified StarTrek.Types.StaffFull.Composer as Composer
import qualified StarTrek.Types.StaffFull.CostumeDepartment as CostumeDepartment
import qualified StarTrek.Types.StaffFull.CostumeDesigner as CostumeDesigner
import qualified StarTrek.Types.StaffFull.DateOfBirth as DateOfBirth
import qualified StarTrek.Types.StaffFull.DateOfDeath as DateOfDeath
import qualified StarTrek.Types.StaffFull.Director as Director
import qualified StarTrek.Types.StaffFull.ExhibitAndAttractionStaff as ExhibitAndAttractionStaff
import qualified StarTrek.Types.StaffFull.FilmEditor as FilmEditor
import qualified StarTrek.Types.StaffFull.GameArtist as GameArtist
import qualified StarTrek.Types.StaffFull.GameAuthor as GameAuthor
import qualified StarTrek.Types.StaffFull.IlmProductionStaff as IlmProductionStaff
import qualified StarTrek.Types.StaffFull.Linguist as Linguist
import qualified StarTrek.Types.StaffFull.LocationStaff as LocationStaff
import qualified StarTrek.Types.StaffFull.MakeupStaff as MakeupStaff
import qualified StarTrek.Types.StaffFull.MusicDepartment as MusicDepartment
import qualified StarTrek.Types.StaffFull.Name as Name
import qualified StarTrek.Types.StaffFull.NovelArtist as NovelArtist
import qualified StarTrek.Types.StaffFull.NovelAuthor as NovelAuthor
import qualified StarTrek.Types.StaffFull.PersonalAssistant as PersonalAssistant
import qualified StarTrek.Types.StaffFull.PlaceOfBirth as PlaceOfBirth
import qualified StarTrek.Types.StaffFull.PlaceOfDeath as PlaceOfDeath
import qualified StarTrek.Types.StaffFull.Producer as Producer
import qualified StarTrek.Types.StaffFull.ProductionAssociate as ProductionAssociate
import qualified StarTrek.Types.StaffFull.ProductionDesigner as ProductionDesigner
import qualified StarTrek.Types.StaffFull.ProductionStaff as ProductionStaff
import qualified StarTrek.Types.StaffFull.PublicationArtist as PublicationArtist
import qualified StarTrek.Types.StaffFull.PublicationDesigner as PublicationDesigner
import qualified StarTrek.Types.StaffFull.PublicationEditor as PublicationEditor
import qualified StarTrek.Types.StaffFull.PublicationStaff as PublicationStaff
import qualified StarTrek.Types.StaffFull.PublicityArtist as PublicityArtist
import qualified StarTrek.Types.StaffFull.ReferenceArtist as ReferenceArtist
import qualified StarTrek.Types.StaffFull.ReferenceAuthor as ReferenceAuthor
import qualified StarTrek.Types.StaffFull.ScienceConsultant as ScienceConsultant
import qualified StarTrek.Types.StaffFull.SoundDepartment as SoundDepartment
import qualified StarTrek.Types.StaffFull.SpecialAndVisualEffectsStaff as SpecialAndVisualEffectsStaff
import qualified StarTrek.Types.StaffFull.SpecialFeaturesStaff as SpecialFeaturesStaff
import qualified StarTrek.Types.StaffFull.StoryEditor as StoryEditor
import qualified StarTrek.Types.StaffFull.StudioExecutive as StudioExecutive
import qualified StarTrek.Types.StaffFull.StuntDepartment as StuntDepartment
import qualified StarTrek.Types.StaffFull.TransportationDepartment as TransportationDepartment
import qualified StarTrek.Types.StaffFull.Uid as Uid
import qualified StarTrek.Types.StaffFull.VideoGameProductionStaff as VideoGameProductionStaff
import qualified StarTrek.Types.StaffFull.Writer as Writer

data StaffFull = StaffFull
  { studioExecutive :: Maybe StudioExecutive.StudioExecutive -- ^ Whether this person is a studio executive
  , publicityArtist :: Maybe PublicityArtist.PublicityArtist -- ^ Whether this person is a publicity artist
  , teleplayAuthoredEpisodes :: Maybe [EpisodeBase.EpisodeBase] -- ^ Base episode, returned in search results
  , storyAuthoredEpisodes :: Maybe [EpisodeBase.EpisodeBase] -- ^ Base episode, returned in search results
  , movies :: Maybe [MovieBase.MovieBase] -- ^ Base movie, returned in search results
  , storyAuthoredMovies :: Maybe [MovieBase.MovieBase] -- ^ Base movie, returned in search results
  , producedMovies :: Maybe [MovieBase.MovieBase] -- ^ Base movie, returned in search results
  , costumeDepartment :: Maybe CostumeDepartment.CostumeDepartment -- ^ Whether this person is from costume department
  , locationStaff :: Maybe LocationStaff.LocationStaff -- ^ Whether this person is a location staff
  , scienceConsultant :: Maybe ScienceConsultant.ScienceConsultant -- ^ Whether this person is a science consultant
  , castingDepartment :: Maybe CastingDepartment.CastingDepartment -- ^ Whether this person is from casting department
  , novelAuthor :: Maybe NovelAuthor.NovelAuthor -- ^ Whether this person is a novel author
  , birthName :: Maybe BirthName.BirthName -- ^ Staff birth name
  , storyEditor :: Maybe StoryEditor.StoryEditor -- ^ Whether this person is a story editor
  , comicLetterArtist :: Maybe ComicLetterArtist.ComicLetterArtist -- ^ Whether this person is a comic letter artist
  , linguist :: Maybe Linguist.Linguist -- ^ Whether this person is a linguist
  , writtenMovies :: Maybe [MovieBase.MovieBase] -- ^ Base movie, returned in search results
  , costumeDesigner :: Maybe CostumeDesigner.CostumeDesigner -- ^ Whether this person is a custume designer
  , specialFeaturesStaff :: Maybe SpecialFeaturesStaff.SpecialFeaturesStaff -- ^ Whether this person is a special features artist
  , dateOfBirth :: Maybe DateOfBirth.DateOfBirth -- ^ Date the staff was born
  , placeOfBirth :: Maybe PlaceOfBirth.PlaceOfBirth -- ^ Place the staff was born
  , productionAssociate :: Maybe ProductionAssociate.ProductionAssociate -- ^ Whether this person is a production associate
  , referenceAuthor :: Maybe ReferenceAuthor.ReferenceAuthor -- ^ Whether this person is a reference author
  , uid :: Uid.Uid -- ^ Staff unique ID
  , directedMovies :: Maybe [MovieBase.MovieBase] -- ^ Base movie, returned in search results
  , gameArtist :: Maybe GameArtist.GameArtist -- ^ Whether this person is a game artist
  , publicationStaff :: Maybe PublicationStaff.PublicationStaff -- ^ Whether this person is a publication staff
  , assistantOrSecondUnitDirector :: Maybe AssistantOrSecondUnitDirector.AssistantOrSecondUnitDirector -- ^ Whether this person is an assistant or secound unit director director
  , soundDepartment :: Maybe SoundDepartment.SoundDepartment -- ^ Whether this person is from sound department
  , publicationEditor :: Maybe PublicationEditor.PublicationEditor -- ^ Whether this person is a publication editor
  , audioAuthor :: Maybe AudioAuthor.AudioAuthor -- ^ Whether this person is an audio author
  , comicPencilArtist :: Maybe ComicPencilArtist.ComicPencilArtist -- ^ Whether this person is a comic pencil artist
  , director :: Maybe Director.Director -- ^ Whether this person is a director
  , videoGameProductionStaff :: Maybe VideoGameProductionStaff.VideoGameProductionStaff -- ^ Whether this person is video game production staff
  , author :: Maybe Author.Author -- ^ Whether this person is an author
  , cbsDigitalStaff :: Maybe CbsDigitalStaff.CbsDigitalStaff -- ^ Whether this person is a part of CBS digital staff
  , comicInkArtist :: Maybe ComicInkArtist.ComicInkArtist -- ^ Whether this person is a comic ink artist
  , producer :: Maybe Producer.Producer -- ^ Whether this person is a producer
  , makeupStaff :: Maybe MakeupStaff.MakeupStaff -- ^ Whether this person is a make-up staff
  , comicAuthor :: Maybe ComicAuthor.ComicAuthor -- ^ Whether this person is a comic author
  , comicArtist :: Maybe ComicArtist.ComicArtist -- ^ Whether this person is a comic artist
  , exhibitAndAttractionStaff :: Maybe ExhibitAndAttractionStaff.ExhibitAndAttractionStaff -- ^ Whether this person is an exhibit and attraction staff
  , artDepartment :: Maybe ArtDepartment.ArtDepartment -- ^ Whether this person is from art department
  , transportationDepartment :: Maybe TransportationDepartment.TransportationDepartment -- ^ Whether this person is from transportation department
  , episodes :: Maybe [EpisodeBase.EpisodeBase] -- ^ Base episode, returned in search results
  , composer :: Maybe Composer.Composer -- ^ Whether this person is a composer
  , directedEpisodes :: Maybe [EpisodeBase.EpisodeBase] -- ^ Base episode, returned in search results
  , comicColorArtist :: Maybe ComicColorArtist.ComicColorArtist -- ^ Whether this person is a comic color artist
  , gameAuthor :: Maybe GameAuthor.GameAuthor -- ^ Whether this person is a game author
  , personalAssistant :: Maybe PersonalAssistant.PersonalAssistant -- ^ Whether this person is a personal assistant
  , referenceArtist :: Maybe ReferenceArtist.ReferenceArtist -- ^ Whether this person is a reference artist
  , productionDesigner :: Maybe ProductionDesigner.ProductionDesigner -- ^ Whether this person is a production designer
  , writtenEpisodes :: Maybe [EpisodeBase.EpisodeBase] -- ^ Base episode, returned in search results
  , filmEditor :: Maybe FilmEditor.FilmEditor -- ^ Whether this person is a film editor
  , gender :: Maybe Gender.Gender -- ^ Gender
  , stuntDepartment :: Maybe StuntDepartment.StuntDepartment -- ^ Whether this person is from stunt department
  , cameraAndElectricalDepartment :: Maybe CameraAndElectricalDepartment.CameraAndElectricalDepartment -- ^ Whether this person is from camera and electrical department
  , ilmProductionStaff :: Maybe IlmProductionStaff.IlmProductionStaff -- ^ Whether this person is a part of ILM production staff
  , screenplayAuthoredMovies :: Maybe [MovieBase.MovieBase] -- ^ Base movie, returned in search results
  , publicationArtist :: Maybe PublicationArtist.PublicationArtist -- ^ Whether this person is a publication artist
  , placeOfDeath :: Maybe PlaceOfDeath.PlaceOfDeath -- ^ Place the staff died
  , dateOfDeath :: Maybe DateOfDeath.DateOfDeath -- ^ Date the staff died
  , cinematographer :: Maybe Cinematographer.Cinematographer -- ^ Whether this person is a cinematographer
  , novelArtist :: Maybe NovelArtist.NovelArtist -- ^ Whether this person is a novel artist
  , writer :: Maybe Writer.Writer -- ^ Whether this person is a writer
  , publicationDesigner :: Maybe PublicationDesigner.PublicationDesigner -- ^ Whether this person is a publication designer
  , productionStaff :: Maybe ProductionStaff.ProductionStaff -- ^ Whether this person is a production staff
  , comicStripArtist :: Maybe ComicStripArtist.ComicStripArtist -- ^ Whether this person is a comic strip artist
  , calendarArtist :: Maybe CalendarArtist.CalendarArtist -- ^ Whether this person is a calendar artist
  , name :: Name.Name -- ^ Staff name
  , comicInteriorArtist :: Maybe ComicInteriorArtist.ComicInteriorArtist -- ^ Whether this person is a comic interior artist
  , specialAndVisualEffectsStaff :: Maybe SpecialAndVisualEffectsStaff.SpecialAndVisualEffectsStaff -- ^ Whether this person is a special and visual effects staff
  , artDirector :: Maybe ArtDirector.ArtDirector -- ^ Whether this person is an art director
  , musicDepartment :: Maybe MusicDepartment.MusicDepartment -- ^ Whether this person is from music department
  }
  deriving (Eq, Show)

staffFullSchema :: FC.Fleece schema => schema StaffFull
staffFullSchema =
  FC.object $
    FC.constructor StaffFull
      #+ FC.optional "studioExecutive" studioExecutive StudioExecutive.studioExecutiveSchema
      #+ FC.optional "publicityArtist" publicityArtist PublicityArtist.publicityArtistSchema
      #+ FC.optional "teleplayAuthoredEpisodes" teleplayAuthoredEpisodes (FC.list EpisodeBase.episodeBaseSchema)
      #+ FC.optional "storyAuthoredEpisodes" storyAuthoredEpisodes (FC.list EpisodeBase.episodeBaseSchema)
      #+ FC.optional "movies" movies (FC.list MovieBase.movieBaseSchema)
      #+ FC.optional "storyAuthoredMovies" storyAuthoredMovies (FC.list MovieBase.movieBaseSchema)
      #+ FC.optional "producedMovies" producedMovies (FC.list MovieBase.movieBaseSchema)
      #+ FC.optional "costumeDepartment" costumeDepartment CostumeDepartment.costumeDepartmentSchema
      #+ FC.optional "locationStaff" locationStaff LocationStaff.locationStaffSchema
      #+ FC.optional "scienceConsultant" scienceConsultant ScienceConsultant.scienceConsultantSchema
      #+ FC.optional "castingDepartment" castingDepartment CastingDepartment.castingDepartmentSchema
      #+ FC.optional "novelAuthor" novelAuthor NovelAuthor.novelAuthorSchema
      #+ FC.optional "birthName" birthName BirthName.birthNameSchema
      #+ FC.optional "storyEditor" storyEditor StoryEditor.storyEditorSchema
      #+ FC.optional "comicLetterArtist" comicLetterArtist ComicLetterArtist.comicLetterArtistSchema
      #+ FC.optional "linguist" linguist Linguist.linguistSchema
      #+ FC.optional "writtenMovies" writtenMovies (FC.list MovieBase.movieBaseSchema)
      #+ FC.optional "costumeDesigner" costumeDesigner CostumeDesigner.costumeDesignerSchema
      #+ FC.optional "specialFeaturesStaff" specialFeaturesStaff SpecialFeaturesStaff.specialFeaturesStaffSchema
      #+ FC.optional "dateOfBirth" dateOfBirth DateOfBirth.dateOfBirthSchema
      #+ FC.optional "placeOfBirth" placeOfBirth PlaceOfBirth.placeOfBirthSchema
      #+ FC.optional "productionAssociate" productionAssociate ProductionAssociate.productionAssociateSchema
      #+ FC.optional "referenceAuthor" referenceAuthor ReferenceAuthor.referenceAuthorSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "directedMovies" directedMovies (FC.list MovieBase.movieBaseSchema)
      #+ FC.optional "gameArtist" gameArtist GameArtist.gameArtistSchema
      #+ FC.optional "publicationStaff" publicationStaff PublicationStaff.publicationStaffSchema
      #+ FC.optional "assistantOrSecondUnitDirector" assistantOrSecondUnitDirector AssistantOrSecondUnitDirector.assistantOrSecondUnitDirectorSchema
      #+ FC.optional "soundDepartment" soundDepartment SoundDepartment.soundDepartmentSchema
      #+ FC.optional "publicationEditor" publicationEditor PublicationEditor.publicationEditorSchema
      #+ FC.optional "audioAuthor" audioAuthor AudioAuthor.audioAuthorSchema
      #+ FC.optional "comicPencilArtist" comicPencilArtist ComicPencilArtist.comicPencilArtistSchema
      #+ FC.optional "director" director Director.directorSchema
      #+ FC.optional "videoGameProductionStaff" videoGameProductionStaff VideoGameProductionStaff.videoGameProductionStaffSchema
      #+ FC.optional "author" author Author.authorSchema
      #+ FC.optional "cbsDigitalStaff" cbsDigitalStaff CbsDigitalStaff.cbsDigitalStaffSchema
      #+ FC.optional "comicInkArtist" comicInkArtist ComicInkArtist.comicInkArtistSchema
      #+ FC.optional "producer" producer Producer.producerSchema
      #+ FC.optional "makeupStaff" makeupStaff MakeupStaff.makeupStaffSchema
      #+ FC.optional "comicAuthor" comicAuthor ComicAuthor.comicAuthorSchema
      #+ FC.optional "comicArtist" comicArtist ComicArtist.comicArtistSchema
      #+ FC.optional "exhibitAndAttractionStaff" exhibitAndAttractionStaff ExhibitAndAttractionStaff.exhibitAndAttractionStaffSchema
      #+ FC.optional "artDepartment" artDepartment ArtDepartment.artDepartmentSchema
      #+ FC.optional "transportationDepartment" transportationDepartment TransportationDepartment.transportationDepartmentSchema
      #+ FC.optional "episodes" episodes (FC.list EpisodeBase.episodeBaseSchema)
      #+ FC.optional "composer" composer Composer.composerSchema
      #+ FC.optional "directedEpisodes" directedEpisodes (FC.list EpisodeBase.episodeBaseSchema)
      #+ FC.optional "comicColorArtist" comicColorArtist ComicColorArtist.comicColorArtistSchema
      #+ FC.optional "gameAuthor" gameAuthor GameAuthor.gameAuthorSchema
      #+ FC.optional "personalAssistant" personalAssistant PersonalAssistant.personalAssistantSchema
      #+ FC.optional "referenceArtist" referenceArtist ReferenceArtist.referenceArtistSchema
      #+ FC.optional "productionDesigner" productionDesigner ProductionDesigner.productionDesignerSchema
      #+ FC.optional "writtenEpisodes" writtenEpisodes (FC.list EpisodeBase.episodeBaseSchema)
      #+ FC.optional "filmEditor" filmEditor FilmEditor.filmEditorSchema
      #+ FC.optional "gender" gender Gender.genderSchema
      #+ FC.optional "stuntDepartment" stuntDepartment StuntDepartment.stuntDepartmentSchema
      #+ FC.optional "cameraAndElectricalDepartment" cameraAndElectricalDepartment CameraAndElectricalDepartment.cameraAndElectricalDepartmentSchema
      #+ FC.optional "ilmProductionStaff" ilmProductionStaff IlmProductionStaff.ilmProductionStaffSchema
      #+ FC.optional "screenplayAuthoredMovies" screenplayAuthoredMovies (FC.list MovieBase.movieBaseSchema)
      #+ FC.optional "publicationArtist" publicationArtist PublicationArtist.publicationArtistSchema
      #+ FC.optional "placeOfDeath" placeOfDeath PlaceOfDeath.placeOfDeathSchema
      #+ FC.optional "dateOfDeath" dateOfDeath DateOfDeath.dateOfDeathSchema
      #+ FC.optional "cinematographer" cinematographer Cinematographer.cinematographerSchema
      #+ FC.optional "novelArtist" novelArtist NovelArtist.novelArtistSchema
      #+ FC.optional "writer" writer Writer.writerSchema
      #+ FC.optional "publicationDesigner" publicationDesigner PublicationDesigner.publicationDesignerSchema
      #+ FC.optional "productionStaff" productionStaff ProductionStaff.productionStaffSchema
      #+ FC.optional "comicStripArtist" comicStripArtist ComicStripArtist.comicStripArtistSchema
      #+ FC.optional "calendarArtist" calendarArtist CalendarArtist.calendarArtistSchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "comicInteriorArtist" comicInteriorArtist ComicInteriorArtist.comicInteriorArtistSchema
      #+ FC.optional "specialAndVisualEffectsStaff" specialAndVisualEffectsStaff SpecialAndVisualEffectsStaff.specialAndVisualEffectsStaffSchema
      #+ FC.optional "artDirector" artDirector ArtDirector.artDirectorSchema
      #+ FC.optional "musicDepartment" musicDepartment MusicDepartment.musicDepartmentSchema