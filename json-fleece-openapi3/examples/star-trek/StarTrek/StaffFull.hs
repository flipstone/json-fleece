{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffFull
  ( StaffFull(..)
  , staffFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.EpisodeBase (EpisodeBase, episodeBaseSchema)
import StarTrek.Gender (Gender, genderSchema)
import StarTrek.MovieBase (MovieBase, movieBaseSchema)
import StarTrek.StaffFull.ArtDepartment (ArtDepartment, artDepartmentSchema)
import StarTrek.StaffFull.ArtDirector (ArtDirector, artDirectorSchema)
import StarTrek.StaffFull.AssistantOrSecondUnitDirector (AssistantOrSecondUnitDirector, assistantOrSecondUnitDirectorSchema)
import StarTrek.StaffFull.AudioAuthor (AudioAuthor, audioAuthorSchema)
import StarTrek.StaffFull.Author (Author, authorSchema)
import StarTrek.StaffFull.BirthName (BirthName, birthNameSchema)
import StarTrek.StaffFull.CalendarArtist (CalendarArtist, calendarArtistSchema)
import StarTrek.StaffFull.CameraAndElectricalDepartment (CameraAndElectricalDepartment, cameraAndElectricalDepartmentSchema)
import StarTrek.StaffFull.CastingDepartment (CastingDepartment, castingDepartmentSchema)
import StarTrek.StaffFull.CbsDigitalStaff (CbsDigitalStaff, cbsDigitalStaffSchema)
import StarTrek.StaffFull.Cinematographer (Cinematographer, cinematographerSchema)
import StarTrek.StaffFull.ComicArtist (ComicArtist, comicArtistSchema)
import StarTrek.StaffFull.ComicAuthor (ComicAuthor, comicAuthorSchema)
import StarTrek.StaffFull.ComicColorArtist (ComicColorArtist, comicColorArtistSchema)
import StarTrek.StaffFull.ComicInkArtist (ComicInkArtist, comicInkArtistSchema)
import StarTrek.StaffFull.ComicInteriorArtist (ComicInteriorArtist, comicInteriorArtistSchema)
import StarTrek.StaffFull.ComicLetterArtist (ComicLetterArtist, comicLetterArtistSchema)
import StarTrek.StaffFull.ComicPencilArtist (ComicPencilArtist, comicPencilArtistSchema)
import StarTrek.StaffFull.ComicStripArtist (ComicStripArtist, comicStripArtistSchema)
import StarTrek.StaffFull.Composer (Composer, composerSchema)
import StarTrek.StaffFull.CostumeDepartment (CostumeDepartment, costumeDepartmentSchema)
import StarTrek.StaffFull.CostumeDesigner (CostumeDesigner, costumeDesignerSchema)
import StarTrek.StaffFull.DateOfBirth (DateOfBirth, dateOfBirthSchema)
import StarTrek.StaffFull.DateOfDeath (DateOfDeath, dateOfDeathSchema)
import StarTrek.StaffFull.Director (Director, directorSchema)
import StarTrek.StaffFull.ExhibitAndAttractionStaff (ExhibitAndAttractionStaff, exhibitAndAttractionStaffSchema)
import StarTrek.StaffFull.FilmEditor (FilmEditor, filmEditorSchema)
import StarTrek.StaffFull.GameArtist (GameArtist, gameArtistSchema)
import StarTrek.StaffFull.GameAuthor (GameAuthor, gameAuthorSchema)
import StarTrek.StaffFull.IlmProductionStaff (IlmProductionStaff, ilmProductionStaffSchema)
import StarTrek.StaffFull.Linguist (Linguist, linguistSchema)
import StarTrek.StaffFull.LocationStaff (LocationStaff, locationStaffSchema)
import StarTrek.StaffFull.MakeupStaff (MakeupStaff, makeupStaffSchema)
import StarTrek.StaffFull.MusicDepartment (MusicDepartment, musicDepartmentSchema)
import StarTrek.StaffFull.Name (Name, nameSchema)
import StarTrek.StaffFull.NovelArtist (NovelArtist, novelArtistSchema)
import StarTrek.StaffFull.NovelAuthor (NovelAuthor, novelAuthorSchema)
import StarTrek.StaffFull.PersonalAssistant (PersonalAssistant, personalAssistantSchema)
import StarTrek.StaffFull.PlaceOfBirth (PlaceOfBirth, placeOfBirthSchema)
import StarTrek.StaffFull.PlaceOfDeath (PlaceOfDeath, placeOfDeathSchema)
import StarTrek.StaffFull.Producer (Producer, producerSchema)
import StarTrek.StaffFull.ProductionAssociate (ProductionAssociate, productionAssociateSchema)
import StarTrek.StaffFull.ProductionDesigner (ProductionDesigner, productionDesignerSchema)
import StarTrek.StaffFull.ProductionStaff (ProductionStaff, productionStaffSchema)
import StarTrek.StaffFull.PublicationArtist (PublicationArtist, publicationArtistSchema)
import StarTrek.StaffFull.PublicationDesigner (PublicationDesigner, publicationDesignerSchema)
import StarTrek.StaffFull.PublicationEditor (PublicationEditor, publicationEditorSchema)
import StarTrek.StaffFull.PublicationStaff (PublicationStaff, publicationStaffSchema)
import StarTrek.StaffFull.PublicityArtist (PublicityArtist, publicityArtistSchema)
import StarTrek.StaffFull.ReferenceArtist (ReferenceArtist, referenceArtistSchema)
import StarTrek.StaffFull.ReferenceAuthor (ReferenceAuthor, referenceAuthorSchema)
import StarTrek.StaffFull.ScienceConsultant (ScienceConsultant, scienceConsultantSchema)
import StarTrek.StaffFull.SoundDepartment (SoundDepartment, soundDepartmentSchema)
import StarTrek.StaffFull.SpecialAndVisualEffectsStaff (SpecialAndVisualEffectsStaff, specialAndVisualEffectsStaffSchema)
import StarTrek.StaffFull.SpecialFeaturesStaff (SpecialFeaturesStaff, specialFeaturesStaffSchema)
import StarTrek.StaffFull.StoryEditor (StoryEditor, storyEditorSchema)
import StarTrek.StaffFull.StudioExecutive (StudioExecutive, studioExecutiveSchema)
import StarTrek.StaffFull.StuntDepartment (StuntDepartment, stuntDepartmentSchema)
import StarTrek.StaffFull.TransportationDepartment (TransportationDepartment, transportationDepartmentSchema)
import StarTrek.StaffFull.Uid (Uid, uidSchema)
import StarTrek.StaffFull.VideoGameProductionStaff (VideoGameProductionStaff, videoGameProductionStaffSchema)
import StarTrek.StaffFull.Writer (Writer, writerSchema)

data StaffFull = StaffFull
  { studioExecutive :: Maybe StudioExecutive -- ^ Whether this person is a studio executive
  , birthName :: Maybe BirthName -- ^ Staff birth name
  , gameArtist :: Maybe GameArtist -- ^ Whether this person is a game artist
  , filmEditor :: Maybe FilmEditor -- ^ Whether this person is a film editor
  , comicStripArtist :: Maybe ComicStripArtist -- ^ Whether this person is a comic strip artist
  , castingDepartment :: Maybe CastingDepartment -- ^ Whether this person is from casting department
  , costumeDesigner :: Maybe CostumeDesigner -- ^ Whether this person is a custume designer
  , author :: Maybe Author -- ^ Whether this person is an author
  , name :: Name -- ^ Staff name
  , personalAssistant :: Maybe PersonalAssistant -- ^ Whether this person is a personal assistant
  , producedMovies :: Maybe [MovieBase] -- ^ Base movie, returned in search results
  , assistantOrSecondUnitDirector :: Maybe AssistantOrSecondUnitDirector -- ^ Whether this person is an assistant or secound unit director director
  , gameAuthor :: Maybe GameAuthor -- ^ Whether this person is a game author
  , publicityArtist :: Maybe PublicityArtist -- ^ Whether this person is a publicity artist
  , directedMovies :: Maybe [MovieBase] -- ^ Base movie, returned in search results
  , teleplayAuthoredEpisodes :: Maybe [EpisodeBase] -- ^ Base episode, returned in search results
  , publicationStaff :: Maybe PublicationStaff -- ^ Whether this person is a publication staff
  , soundDepartment :: Maybe SoundDepartment -- ^ Whether this person is from sound department
  , composer :: Maybe Composer -- ^ Whether this person is a composer
  , publicationArtist :: Maybe PublicationArtist -- ^ Whether this person is a publication artist
  , referenceArtist :: Maybe ReferenceArtist -- ^ Whether this person is a reference artist
  , writtenMovies :: Maybe [MovieBase] -- ^ Base movie, returned in search results
  , comicColorArtist :: Maybe ComicColorArtist -- ^ Whether this person is a comic color artist
  , comicAuthor :: Maybe ComicAuthor -- ^ Whether this person is a comic author
  , director :: Maybe Director -- ^ Whether this person is a director
  , artDepartment :: Maybe ArtDepartment -- ^ Whether this person is from art department
  , writtenEpisodes :: Maybe [EpisodeBase] -- ^ Base episode, returned in search results
  , publicationDesigner :: Maybe PublicationDesigner -- ^ Whether this person is a publication designer
  , comicInkArtist :: Maybe ComicInkArtist -- ^ Whether this person is a comic ink artist
  , comicArtist :: Maybe ComicArtist -- ^ Whether this person is a comic artist
  , referenceAuthor :: Maybe ReferenceAuthor -- ^ Whether this person is a reference author
  , cameraAndElectricalDepartment :: Maybe CameraAndElectricalDepartment -- ^ Whether this person is from camera and electrical department
  , exhibitAndAttractionStaff :: Maybe ExhibitAndAttractionStaff -- ^ Whether this person is an exhibit and attraction staff
  , stuntDepartment :: Maybe StuntDepartment -- ^ Whether this person is from stunt department
  , uid :: Uid -- ^ Staff unique ID
  , writer :: Maybe Writer -- ^ Whether this person is a writer
  , audioAuthor :: Maybe AudioAuthor -- ^ Whether this person is an audio author
  , episodes :: Maybe [EpisodeBase] -- ^ Base episode, returned in search results
  , transportationDepartment :: Maybe TransportationDepartment -- ^ Whether this person is from transportation department
  , screenplayAuthoredMovies :: Maybe [MovieBase] -- ^ Base movie, returned in search results
  , linguist :: Maybe Linguist -- ^ Whether this person is a linguist
  , cinematographer :: Maybe Cinematographer -- ^ Whether this person is a cinematographer
  , dateOfDeath :: Maybe DateOfDeath -- ^ Date the staff died
  , locationStaff :: Maybe LocationStaff -- ^ Whether this person is a location staff
  , storyAuthoredMovies :: Maybe [MovieBase] -- ^ Base movie, returned in search results
  , calendarArtist :: Maybe CalendarArtist -- ^ Whether this person is a calendar artist
  , costumeDepartment :: Maybe CostumeDepartment -- ^ Whether this person is from costume department
  , novelArtist :: Maybe NovelArtist -- ^ Whether this person is a novel artist
  , artDirector :: Maybe ArtDirector -- ^ Whether this person is an art director
  , directedEpisodes :: Maybe [EpisodeBase] -- ^ Base episode, returned in search results
  , productionAssociate :: Maybe ProductionAssociate -- ^ Whether this person is a production associate
  , cbsDigitalStaff :: Maybe CbsDigitalStaff -- ^ Whether this person is a part of CBS digital staff
  , storyEditor :: Maybe StoryEditor -- ^ Whether this person is a story editor
  , comicInteriorArtist :: Maybe ComicInteriorArtist -- ^ Whether this person is a comic interior artist
  , specialFeaturesStaff :: Maybe SpecialFeaturesStaff -- ^ Whether this person is a special features artist
  , scienceConsultant :: Maybe ScienceConsultant -- ^ Whether this person is a science consultant
  , gender :: Maybe Gender -- ^ Gender
  , dateOfBirth :: Maybe DateOfBirth -- ^ Date the staff was born
  , publicationEditor :: Maybe PublicationEditor -- ^ Whether this person is a publication editor
  , placeOfDeath :: Maybe PlaceOfDeath -- ^ Place the staff died
  , novelAuthor :: Maybe NovelAuthor -- ^ Whether this person is a novel author
  , ilmProductionStaff :: Maybe IlmProductionStaff -- ^ Whether this person is a part of ILM production staff
  , producer :: Maybe Producer -- ^ Whether this person is a producer
  , musicDepartment :: Maybe MusicDepartment -- ^ Whether this person is from music department
  , comicPencilArtist :: Maybe ComicPencilArtist -- ^ Whether this person is a comic pencil artist
  , specialAndVisualEffectsStaff :: Maybe SpecialAndVisualEffectsStaff -- ^ Whether this person is a special and visual effects staff
  , makeupStaff :: Maybe MakeupStaff -- ^ Whether this person is a make-up staff
  , movies :: Maybe [MovieBase] -- ^ Base movie, returned in search results
  , storyAuthoredEpisodes :: Maybe [EpisodeBase] -- ^ Base episode, returned in search results
  , comicLetterArtist :: Maybe ComicLetterArtist -- ^ Whether this person is a comic letter artist
  , placeOfBirth :: Maybe PlaceOfBirth -- ^ Place the staff was born
  , productionDesigner :: Maybe ProductionDesigner -- ^ Whether this person is a production designer
  , videoGameProductionStaff :: Maybe VideoGameProductionStaff -- ^ Whether this person is video game production staff
  , productionStaff :: Maybe ProductionStaff -- ^ Whether this person is a production staff
  }
  deriving (Eq, Show)

staffFullSchema :: FC.Fleece schema => schema StaffFull
staffFullSchema =
  FC.object $
    FC.constructor StaffFull
      #+ FC.optional "studioExecutive" studioExecutive studioExecutiveSchema
      #+ FC.optional "birthName" birthName birthNameSchema
      #+ FC.optional "gameArtist" gameArtist gameArtistSchema
      #+ FC.optional "filmEditor" filmEditor filmEditorSchema
      #+ FC.optional "comicStripArtist" comicStripArtist comicStripArtistSchema
      #+ FC.optional "castingDepartment" castingDepartment castingDepartmentSchema
      #+ FC.optional "costumeDesigner" costumeDesigner costumeDesignerSchema
      #+ FC.optional "author" author authorSchema
      #+ FC.required "name" name nameSchema
      #+ FC.optional "personalAssistant" personalAssistant personalAssistantSchema
      #+ FC.optional "producedMovies" producedMovies (FC.list movieBaseSchema)
      #+ FC.optional "assistantOrSecondUnitDirector" assistantOrSecondUnitDirector assistantOrSecondUnitDirectorSchema
      #+ FC.optional "gameAuthor" gameAuthor gameAuthorSchema
      #+ FC.optional "publicityArtist" publicityArtist publicityArtistSchema
      #+ FC.optional "directedMovies" directedMovies (FC.list movieBaseSchema)
      #+ FC.optional "teleplayAuthoredEpisodes" teleplayAuthoredEpisodes (FC.list episodeBaseSchema)
      #+ FC.optional "publicationStaff" publicationStaff publicationStaffSchema
      #+ FC.optional "soundDepartment" soundDepartment soundDepartmentSchema
      #+ FC.optional "composer" composer composerSchema
      #+ FC.optional "publicationArtist" publicationArtist publicationArtistSchema
      #+ FC.optional "referenceArtist" referenceArtist referenceArtistSchema
      #+ FC.optional "writtenMovies" writtenMovies (FC.list movieBaseSchema)
      #+ FC.optional "comicColorArtist" comicColorArtist comicColorArtistSchema
      #+ FC.optional "comicAuthor" comicAuthor comicAuthorSchema
      #+ FC.optional "director" director directorSchema
      #+ FC.optional "artDepartment" artDepartment artDepartmentSchema
      #+ FC.optional "writtenEpisodes" writtenEpisodes (FC.list episodeBaseSchema)
      #+ FC.optional "publicationDesigner" publicationDesigner publicationDesignerSchema
      #+ FC.optional "comicInkArtist" comicInkArtist comicInkArtistSchema
      #+ FC.optional "comicArtist" comicArtist comicArtistSchema
      #+ FC.optional "referenceAuthor" referenceAuthor referenceAuthorSchema
      #+ FC.optional "cameraAndElectricalDepartment" cameraAndElectricalDepartment cameraAndElectricalDepartmentSchema
      #+ FC.optional "exhibitAndAttractionStaff" exhibitAndAttractionStaff exhibitAndAttractionStaffSchema
      #+ FC.optional "stuntDepartment" stuntDepartment stuntDepartmentSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "writer" writer writerSchema
      #+ FC.optional "audioAuthor" audioAuthor audioAuthorSchema
      #+ FC.optional "episodes" episodes (FC.list episodeBaseSchema)
      #+ FC.optional "transportationDepartment" transportationDepartment transportationDepartmentSchema
      #+ FC.optional "screenplayAuthoredMovies" screenplayAuthoredMovies (FC.list movieBaseSchema)
      #+ FC.optional "linguist" linguist linguistSchema
      #+ FC.optional "cinematographer" cinematographer cinematographerSchema
      #+ FC.optional "dateOfDeath" dateOfDeath dateOfDeathSchema
      #+ FC.optional "locationStaff" locationStaff locationStaffSchema
      #+ FC.optional "storyAuthoredMovies" storyAuthoredMovies (FC.list movieBaseSchema)
      #+ FC.optional "calendarArtist" calendarArtist calendarArtistSchema
      #+ FC.optional "costumeDepartment" costumeDepartment costumeDepartmentSchema
      #+ FC.optional "novelArtist" novelArtist novelArtistSchema
      #+ FC.optional "artDirector" artDirector artDirectorSchema
      #+ FC.optional "directedEpisodes" directedEpisodes (FC.list episodeBaseSchema)
      #+ FC.optional "productionAssociate" productionAssociate productionAssociateSchema
      #+ FC.optional "cbsDigitalStaff" cbsDigitalStaff cbsDigitalStaffSchema
      #+ FC.optional "storyEditor" storyEditor storyEditorSchema
      #+ FC.optional "comicInteriorArtist" comicInteriorArtist comicInteriorArtistSchema
      #+ FC.optional "specialFeaturesStaff" specialFeaturesStaff specialFeaturesStaffSchema
      #+ FC.optional "scienceConsultant" scienceConsultant scienceConsultantSchema
      #+ FC.optional "gender" gender genderSchema
      #+ FC.optional "dateOfBirth" dateOfBirth dateOfBirthSchema
      #+ FC.optional "publicationEditor" publicationEditor publicationEditorSchema
      #+ FC.optional "placeOfDeath" placeOfDeath placeOfDeathSchema
      #+ FC.optional "novelAuthor" novelAuthor novelAuthorSchema
      #+ FC.optional "ilmProductionStaff" ilmProductionStaff ilmProductionStaffSchema
      #+ FC.optional "producer" producer producerSchema
      #+ FC.optional "musicDepartment" musicDepartment musicDepartmentSchema
      #+ FC.optional "comicPencilArtist" comicPencilArtist comicPencilArtistSchema
      #+ FC.optional "specialAndVisualEffectsStaff" specialAndVisualEffectsStaff specialAndVisualEffectsStaffSchema
      #+ FC.optional "makeupStaff" makeupStaff makeupStaffSchema
      #+ FC.optional "movies" movies (FC.list movieBaseSchema)
      #+ FC.optional "storyAuthoredEpisodes" storyAuthoredEpisodes (FC.list episodeBaseSchema)
      #+ FC.optional "comicLetterArtist" comicLetterArtist comicLetterArtistSchema
      #+ FC.optional "placeOfBirth" placeOfBirth placeOfBirthSchema
      #+ FC.optional "productionDesigner" productionDesigner productionDesignerSchema
      #+ FC.optional "videoGameProductionStaff" videoGameProductionStaff videoGameProductionStaffSchema
      #+ FC.optional "productionStaff" productionStaff productionStaffSchema