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
  { artDepartment :: Maybe ArtDepartment.ArtDepartment -- ^ Whether this person is from art department
  , artDirector :: Maybe ArtDirector.ArtDirector -- ^ Whether this person is an art director
  , assistantOrSecondUnitDirector :: Maybe AssistantOrSecondUnitDirector.AssistantOrSecondUnitDirector -- ^ Whether this person is an assistant or secound unit director director
  , audioAuthor :: Maybe AudioAuthor.AudioAuthor -- ^ Whether this person is an audio author
  , author :: Maybe Author.Author -- ^ Whether this person is an author
  , birthName :: Maybe BirthName.BirthName -- ^ Staff birth name
  , calendarArtist :: Maybe CalendarArtist.CalendarArtist -- ^ Whether this person is a calendar artist
  , cameraAndElectricalDepartment :: Maybe CameraAndElectricalDepartment.CameraAndElectricalDepartment -- ^ Whether this person is from camera and electrical department
  , castingDepartment :: Maybe CastingDepartment.CastingDepartment -- ^ Whether this person is from casting department
  , cbsDigitalStaff :: Maybe CbsDigitalStaff.CbsDigitalStaff -- ^ Whether this person is a part of CBS digital staff
  , cinematographer :: Maybe Cinematographer.Cinematographer -- ^ Whether this person is a cinematographer
  , comicArtist :: Maybe ComicArtist.ComicArtist -- ^ Whether this person is a comic artist
  , comicAuthor :: Maybe ComicAuthor.ComicAuthor -- ^ Whether this person is a comic author
  , comicColorArtist :: Maybe ComicColorArtist.ComicColorArtist -- ^ Whether this person is a comic color artist
  , comicInkArtist :: Maybe ComicInkArtist.ComicInkArtist -- ^ Whether this person is a comic ink artist
  , comicInteriorArtist :: Maybe ComicInteriorArtist.ComicInteriorArtist -- ^ Whether this person is a comic interior artist
  , comicLetterArtist :: Maybe ComicLetterArtist.ComicLetterArtist -- ^ Whether this person is a comic letter artist
  , comicPencilArtist :: Maybe ComicPencilArtist.ComicPencilArtist -- ^ Whether this person is a comic pencil artist
  , comicStripArtist :: Maybe ComicStripArtist.ComicStripArtist -- ^ Whether this person is a comic strip artist
  , composer :: Maybe Composer.Composer -- ^ Whether this person is a composer
  , costumeDepartment :: Maybe CostumeDepartment.CostumeDepartment -- ^ Whether this person is from costume department
  , costumeDesigner :: Maybe CostumeDesigner.CostumeDesigner -- ^ Whether this person is a custume designer
  , dateOfBirth :: Maybe DateOfBirth.DateOfBirth -- ^ Date the staff was born
  , dateOfDeath :: Maybe DateOfDeath.DateOfDeath -- ^ Date the staff died
  , directedEpisodes :: Maybe [EpisodeBase.EpisodeBase] -- ^ Base episode, returned in search results
  , directedMovies :: Maybe [MovieBase.MovieBase] -- ^ Base movie, returned in search results
  , director :: Maybe Director.Director -- ^ Whether this person is a director
  , episodes :: Maybe [EpisodeBase.EpisodeBase] -- ^ Base episode, returned in search results
  , exhibitAndAttractionStaff :: Maybe ExhibitAndAttractionStaff.ExhibitAndAttractionStaff -- ^ Whether this person is an exhibit and attraction staff
  , filmEditor :: Maybe FilmEditor.FilmEditor -- ^ Whether this person is a film editor
  , gameArtist :: Maybe GameArtist.GameArtist -- ^ Whether this person is a game artist
  , gameAuthor :: Maybe GameAuthor.GameAuthor -- ^ Whether this person is a game author
  , gender :: Maybe Gender.Gender -- ^ Gender
  , ilmProductionStaff :: Maybe IlmProductionStaff.IlmProductionStaff -- ^ Whether this person is a part of ILM production staff
  , linguist :: Maybe Linguist.Linguist -- ^ Whether this person is a linguist
  , locationStaff :: Maybe LocationStaff.LocationStaff -- ^ Whether this person is a location staff
  , makeupStaff :: Maybe MakeupStaff.MakeupStaff -- ^ Whether this person is a make-up staff
  , movies :: Maybe [MovieBase.MovieBase] -- ^ Base movie, returned in search results
  , musicDepartment :: Maybe MusicDepartment.MusicDepartment -- ^ Whether this person is from music department
  , name :: Name.Name -- ^ Staff name
  , novelArtist :: Maybe NovelArtist.NovelArtist -- ^ Whether this person is a novel artist
  , novelAuthor :: Maybe NovelAuthor.NovelAuthor -- ^ Whether this person is a novel author
  , personalAssistant :: Maybe PersonalAssistant.PersonalAssistant -- ^ Whether this person is a personal assistant
  , placeOfBirth :: Maybe PlaceOfBirth.PlaceOfBirth -- ^ Place the staff was born
  , placeOfDeath :: Maybe PlaceOfDeath.PlaceOfDeath -- ^ Place the staff died
  , producedMovies :: Maybe [MovieBase.MovieBase] -- ^ Base movie, returned in search results
  , producer :: Maybe Producer.Producer -- ^ Whether this person is a producer
  , productionAssociate :: Maybe ProductionAssociate.ProductionAssociate -- ^ Whether this person is a production associate
  , productionDesigner :: Maybe ProductionDesigner.ProductionDesigner -- ^ Whether this person is a production designer
  , productionStaff :: Maybe ProductionStaff.ProductionStaff -- ^ Whether this person is a production staff
  , publicationArtist :: Maybe PublicationArtist.PublicationArtist -- ^ Whether this person is a publication artist
  , publicationDesigner :: Maybe PublicationDesigner.PublicationDesigner -- ^ Whether this person is a publication designer
  , publicationEditor :: Maybe PublicationEditor.PublicationEditor -- ^ Whether this person is a publication editor
  , publicationStaff :: Maybe PublicationStaff.PublicationStaff -- ^ Whether this person is a publication staff
  , publicityArtist :: Maybe PublicityArtist.PublicityArtist -- ^ Whether this person is a publicity artist
  , referenceArtist :: Maybe ReferenceArtist.ReferenceArtist -- ^ Whether this person is a reference artist
  , referenceAuthor :: Maybe ReferenceAuthor.ReferenceAuthor -- ^ Whether this person is a reference author
  , scienceConsultant :: Maybe ScienceConsultant.ScienceConsultant -- ^ Whether this person is a science consultant
  , screenplayAuthoredMovies :: Maybe [MovieBase.MovieBase] -- ^ Base movie, returned in search results
  , soundDepartment :: Maybe SoundDepartment.SoundDepartment -- ^ Whether this person is from sound department
  , specialAndVisualEffectsStaff :: Maybe SpecialAndVisualEffectsStaff.SpecialAndVisualEffectsStaff -- ^ Whether this person is a special and visual effects staff
  , specialFeaturesStaff :: Maybe SpecialFeaturesStaff.SpecialFeaturesStaff -- ^ Whether this person is a special features artist
  , storyAuthoredEpisodes :: Maybe [EpisodeBase.EpisodeBase] -- ^ Base episode, returned in search results
  , storyAuthoredMovies :: Maybe [MovieBase.MovieBase] -- ^ Base movie, returned in search results
  , storyEditor :: Maybe StoryEditor.StoryEditor -- ^ Whether this person is a story editor
  , studioExecutive :: Maybe StudioExecutive.StudioExecutive -- ^ Whether this person is a studio executive
  , stuntDepartment :: Maybe StuntDepartment.StuntDepartment -- ^ Whether this person is from stunt department
  , teleplayAuthoredEpisodes :: Maybe [EpisodeBase.EpisodeBase] -- ^ Base episode, returned in search results
  , transportationDepartment :: Maybe TransportationDepartment.TransportationDepartment -- ^ Whether this person is from transportation department
  , uid :: Uid.Uid -- ^ Staff unique ID
  , videoGameProductionStaff :: Maybe VideoGameProductionStaff.VideoGameProductionStaff -- ^ Whether this person is video game production staff
  , writer :: Maybe Writer.Writer -- ^ Whether this person is a writer
  , writtenEpisodes :: Maybe [EpisodeBase.EpisodeBase] -- ^ Base episode, returned in search results
  , writtenMovies :: Maybe [MovieBase.MovieBase] -- ^ Base movie, returned in search results
  }
  deriving (Eq, Show)

staffFullSchema :: FC.Fleece schema => schema StaffFull
staffFullSchema =
  FC.object $
    FC.constructor StaffFull
      #+ FC.optional "artDepartment" artDepartment ArtDepartment.artDepartmentSchema
      #+ FC.optional "artDirector" artDirector ArtDirector.artDirectorSchema
      #+ FC.optional "assistantOrSecondUnitDirector" assistantOrSecondUnitDirector AssistantOrSecondUnitDirector.assistantOrSecondUnitDirectorSchema
      #+ FC.optional "audioAuthor" audioAuthor AudioAuthor.audioAuthorSchema
      #+ FC.optional "author" author Author.authorSchema
      #+ FC.optional "birthName" birthName BirthName.birthNameSchema
      #+ FC.optional "calendarArtist" calendarArtist CalendarArtist.calendarArtistSchema
      #+ FC.optional "cameraAndElectricalDepartment" cameraAndElectricalDepartment CameraAndElectricalDepartment.cameraAndElectricalDepartmentSchema
      #+ FC.optional "castingDepartment" castingDepartment CastingDepartment.castingDepartmentSchema
      #+ FC.optional "cbsDigitalStaff" cbsDigitalStaff CbsDigitalStaff.cbsDigitalStaffSchema
      #+ FC.optional "cinematographer" cinematographer Cinematographer.cinematographerSchema
      #+ FC.optional "comicArtist" comicArtist ComicArtist.comicArtistSchema
      #+ FC.optional "comicAuthor" comicAuthor ComicAuthor.comicAuthorSchema
      #+ FC.optional "comicColorArtist" comicColorArtist ComicColorArtist.comicColorArtistSchema
      #+ FC.optional "comicInkArtist" comicInkArtist ComicInkArtist.comicInkArtistSchema
      #+ FC.optional "comicInteriorArtist" comicInteriorArtist ComicInteriorArtist.comicInteriorArtistSchema
      #+ FC.optional "comicLetterArtist" comicLetterArtist ComicLetterArtist.comicLetterArtistSchema
      #+ FC.optional "comicPencilArtist" comicPencilArtist ComicPencilArtist.comicPencilArtistSchema
      #+ FC.optional "comicStripArtist" comicStripArtist ComicStripArtist.comicStripArtistSchema
      #+ FC.optional "composer" composer Composer.composerSchema
      #+ FC.optional "costumeDepartment" costumeDepartment CostumeDepartment.costumeDepartmentSchema
      #+ FC.optional "costumeDesigner" costumeDesigner CostumeDesigner.costumeDesignerSchema
      #+ FC.optional "dateOfBirth" dateOfBirth DateOfBirth.dateOfBirthSchema
      #+ FC.optional "dateOfDeath" dateOfDeath DateOfDeath.dateOfDeathSchema
      #+ FC.optional "directedEpisodes" directedEpisodes (FC.list EpisodeBase.episodeBaseSchema)
      #+ FC.optional "directedMovies" directedMovies (FC.list MovieBase.movieBaseSchema)
      #+ FC.optional "director" director Director.directorSchema
      #+ FC.optional "episodes" episodes (FC.list EpisodeBase.episodeBaseSchema)
      #+ FC.optional "exhibitAndAttractionStaff" exhibitAndAttractionStaff ExhibitAndAttractionStaff.exhibitAndAttractionStaffSchema
      #+ FC.optional "filmEditor" filmEditor FilmEditor.filmEditorSchema
      #+ FC.optional "gameArtist" gameArtist GameArtist.gameArtistSchema
      #+ FC.optional "gameAuthor" gameAuthor GameAuthor.gameAuthorSchema
      #+ FC.optional "gender" gender Gender.genderSchema
      #+ FC.optional "ilmProductionStaff" ilmProductionStaff IlmProductionStaff.ilmProductionStaffSchema
      #+ FC.optional "linguist" linguist Linguist.linguistSchema
      #+ FC.optional "locationStaff" locationStaff LocationStaff.locationStaffSchema
      #+ FC.optional "makeupStaff" makeupStaff MakeupStaff.makeupStaffSchema
      #+ FC.optional "movies" movies (FC.list MovieBase.movieBaseSchema)
      #+ FC.optional "musicDepartment" musicDepartment MusicDepartment.musicDepartmentSchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "novelArtist" novelArtist NovelArtist.novelArtistSchema
      #+ FC.optional "novelAuthor" novelAuthor NovelAuthor.novelAuthorSchema
      #+ FC.optional "personalAssistant" personalAssistant PersonalAssistant.personalAssistantSchema
      #+ FC.optional "placeOfBirth" placeOfBirth PlaceOfBirth.placeOfBirthSchema
      #+ FC.optional "placeOfDeath" placeOfDeath PlaceOfDeath.placeOfDeathSchema
      #+ FC.optional "producedMovies" producedMovies (FC.list MovieBase.movieBaseSchema)
      #+ FC.optional "producer" producer Producer.producerSchema
      #+ FC.optional "productionAssociate" productionAssociate ProductionAssociate.productionAssociateSchema
      #+ FC.optional "productionDesigner" productionDesigner ProductionDesigner.productionDesignerSchema
      #+ FC.optional "productionStaff" productionStaff ProductionStaff.productionStaffSchema
      #+ FC.optional "publicationArtist" publicationArtist PublicationArtist.publicationArtistSchema
      #+ FC.optional "publicationDesigner" publicationDesigner PublicationDesigner.publicationDesignerSchema
      #+ FC.optional "publicationEditor" publicationEditor PublicationEditor.publicationEditorSchema
      #+ FC.optional "publicationStaff" publicationStaff PublicationStaff.publicationStaffSchema
      #+ FC.optional "publicityArtist" publicityArtist PublicityArtist.publicityArtistSchema
      #+ FC.optional "referenceArtist" referenceArtist ReferenceArtist.referenceArtistSchema
      #+ FC.optional "referenceAuthor" referenceAuthor ReferenceAuthor.referenceAuthorSchema
      #+ FC.optional "scienceConsultant" scienceConsultant ScienceConsultant.scienceConsultantSchema
      #+ FC.optional "screenplayAuthoredMovies" screenplayAuthoredMovies (FC.list MovieBase.movieBaseSchema)
      #+ FC.optional "soundDepartment" soundDepartment SoundDepartment.soundDepartmentSchema
      #+ FC.optional "specialAndVisualEffectsStaff" specialAndVisualEffectsStaff SpecialAndVisualEffectsStaff.specialAndVisualEffectsStaffSchema
      #+ FC.optional "specialFeaturesStaff" specialFeaturesStaff SpecialFeaturesStaff.specialFeaturesStaffSchema
      #+ FC.optional "storyAuthoredEpisodes" storyAuthoredEpisodes (FC.list EpisodeBase.episodeBaseSchema)
      #+ FC.optional "storyAuthoredMovies" storyAuthoredMovies (FC.list MovieBase.movieBaseSchema)
      #+ FC.optional "storyEditor" storyEditor StoryEditor.storyEditorSchema
      #+ FC.optional "studioExecutive" studioExecutive StudioExecutive.studioExecutiveSchema
      #+ FC.optional "stuntDepartment" stuntDepartment StuntDepartment.stuntDepartmentSchema
      #+ FC.optional "teleplayAuthoredEpisodes" teleplayAuthoredEpisodes (FC.list EpisodeBase.episodeBaseSchema)
      #+ FC.optional "transportationDepartment" transportationDepartment TransportationDepartment.transportationDepartmentSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "videoGameProductionStaff" videoGameProductionStaff VideoGameProductionStaff.videoGameProductionStaffSchema
      #+ FC.optional "writer" writer Writer.writerSchema
      #+ FC.optional "writtenEpisodes" writtenEpisodes (FC.list EpisodeBase.episodeBaseSchema)
      #+ FC.optional "writtenMovies" writtenMovies (FC.list MovieBase.movieBaseSchema)