{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffBase
  ( StaffBase(..)
  , staffBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.Gender (Gender, genderSchema)
import StarTrek.StaffBase.ArtDepartment (ArtDepartment, artDepartmentSchema)
import StarTrek.StaffBase.ArtDirector (ArtDirector, artDirectorSchema)
import StarTrek.StaffBase.AssistantOrSecondUnitDirector (AssistantOrSecondUnitDirector, assistantOrSecondUnitDirectorSchema)
import StarTrek.StaffBase.AudioAuthor (AudioAuthor, audioAuthorSchema)
import StarTrek.StaffBase.Author (Author, authorSchema)
import StarTrek.StaffBase.BirthName (BirthName, birthNameSchema)
import StarTrek.StaffBase.CalendarArtist (CalendarArtist, calendarArtistSchema)
import StarTrek.StaffBase.CameraAndElectricalDepartment (CameraAndElectricalDepartment, cameraAndElectricalDepartmentSchema)
import StarTrek.StaffBase.CastingDepartment (CastingDepartment, castingDepartmentSchema)
import StarTrek.StaffBase.CbsDigitalStaff (CbsDigitalStaff, cbsDigitalStaffSchema)
import StarTrek.StaffBase.Cinematographer (Cinematographer, cinematographerSchema)
import StarTrek.StaffBase.ComicArtist (ComicArtist, comicArtistSchema)
import StarTrek.StaffBase.ComicAuthor (ComicAuthor, comicAuthorSchema)
import StarTrek.StaffBase.ComicColorArtist (ComicColorArtist, comicColorArtistSchema)
import StarTrek.StaffBase.ComicInkArtist (ComicInkArtist, comicInkArtistSchema)
import StarTrek.StaffBase.ComicInteriorArtist (ComicInteriorArtist, comicInteriorArtistSchema)
import StarTrek.StaffBase.ComicLetterArtist (ComicLetterArtist, comicLetterArtistSchema)
import StarTrek.StaffBase.ComicPencilArtist (ComicPencilArtist, comicPencilArtistSchema)
import StarTrek.StaffBase.ComicStripArtist (ComicStripArtist, comicStripArtistSchema)
import StarTrek.StaffBase.Composer (Composer, composerSchema)
import StarTrek.StaffBase.CostumeDepartment (CostumeDepartment, costumeDepartmentSchema)
import StarTrek.StaffBase.CostumeDesigner (CostumeDesigner, costumeDesignerSchema)
import StarTrek.StaffBase.DateOfBirth (DateOfBirth, dateOfBirthSchema)
import StarTrek.StaffBase.DateOfDeath (DateOfDeath, dateOfDeathSchema)
import StarTrek.StaffBase.Director (Director, directorSchema)
import StarTrek.StaffBase.ExhibitAndAttractionStaff (ExhibitAndAttractionStaff, exhibitAndAttractionStaffSchema)
import StarTrek.StaffBase.FilmEditor (FilmEditor, filmEditorSchema)
import StarTrek.StaffBase.GameArtist (GameArtist, gameArtistSchema)
import StarTrek.StaffBase.GameAuthor (GameAuthor, gameAuthorSchema)
import StarTrek.StaffBase.IlmProductionStaff (IlmProductionStaff, ilmProductionStaffSchema)
import StarTrek.StaffBase.Linguist (Linguist, linguistSchema)
import StarTrek.StaffBase.LocationStaff (LocationStaff, locationStaffSchema)
import StarTrek.StaffBase.MakeupStaff (MakeupStaff, makeupStaffSchema)
import StarTrek.StaffBase.MusicDepartment (MusicDepartment, musicDepartmentSchema)
import StarTrek.StaffBase.Name (Name, nameSchema)
import StarTrek.StaffBase.NovelArtist (NovelArtist, novelArtistSchema)
import StarTrek.StaffBase.NovelAuthor (NovelAuthor, novelAuthorSchema)
import StarTrek.StaffBase.PersonalAssistant (PersonalAssistant, personalAssistantSchema)
import StarTrek.StaffBase.PlaceOfBirth (PlaceOfBirth, placeOfBirthSchema)
import StarTrek.StaffBase.PlaceOfDeath (PlaceOfDeath, placeOfDeathSchema)
import StarTrek.StaffBase.Producer (Producer, producerSchema)
import StarTrek.StaffBase.ProductionAssociate (ProductionAssociate, productionAssociateSchema)
import StarTrek.StaffBase.ProductionDesigner (ProductionDesigner, productionDesignerSchema)
import StarTrek.StaffBase.ProductionStaff (ProductionStaff, productionStaffSchema)
import StarTrek.StaffBase.PublicationArtist (PublicationArtist, publicationArtistSchema)
import StarTrek.StaffBase.PublicationDesigner (PublicationDesigner, publicationDesignerSchema)
import StarTrek.StaffBase.PublicationEditor (PublicationEditor, publicationEditorSchema)
import StarTrek.StaffBase.PublicationStaff (PublicationStaff, publicationStaffSchema)
import StarTrek.StaffBase.PublicityArtist (PublicityArtist, publicityArtistSchema)
import StarTrek.StaffBase.ReferenceArtist (ReferenceArtist, referenceArtistSchema)
import StarTrek.StaffBase.ReferenceAuthor (ReferenceAuthor, referenceAuthorSchema)
import StarTrek.StaffBase.ScienceConsultant (ScienceConsultant, scienceConsultantSchema)
import StarTrek.StaffBase.SoundDepartment (SoundDepartment, soundDepartmentSchema)
import StarTrek.StaffBase.SpecialAndVisualEffectsStaff (SpecialAndVisualEffectsStaff, specialAndVisualEffectsStaffSchema)
import StarTrek.StaffBase.SpecialFeaturesStaff (SpecialFeaturesStaff, specialFeaturesStaffSchema)
import StarTrek.StaffBase.StoryEditor (StoryEditor, storyEditorSchema)
import StarTrek.StaffBase.StudioExecutive (StudioExecutive, studioExecutiveSchema)
import StarTrek.StaffBase.StuntDepartment (StuntDepartment, stuntDepartmentSchema)
import StarTrek.StaffBase.TransportationDepartment (TransportationDepartment, transportationDepartmentSchema)
import StarTrek.StaffBase.Uid (Uid, uidSchema)
import StarTrek.StaffBase.VideoGameProductionStaff (VideoGameProductionStaff, videoGameProductionStaffSchema)
import StarTrek.StaffBase.Writer (Writer, writerSchema)

data StaffBase = StaffBase
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
  , assistantOrSecondUnitDirector :: Maybe AssistantOrSecondUnitDirector -- ^ Whether this person is an assistant or secound unit director director
  , gameAuthor :: Maybe GameAuthor -- ^ Whether this person is a game author
  , publicityArtist :: Maybe PublicityArtist -- ^ Whether this person is a publication artist
  , publicationStaff :: Maybe PublicationStaff -- ^ Whether this person is a publication staff
  , soundDepartment :: Maybe SoundDepartment -- ^ Whether this person is from sound department
  , composer :: Maybe Composer -- ^ Whether this person is a composer
  , publicationArtist :: Maybe PublicationArtist -- ^ Whether this person is a publication artist
  , referenceArtist :: Maybe ReferenceArtist -- ^ Whether this person is a reference artist
  , comicColorArtist :: Maybe ComicColorArtist -- ^ Whether this person is a comic color artist
  , comicAuthor :: Maybe ComicAuthor -- ^ Whether this person is a comic author
  , director :: Maybe Director -- ^ Whether this person is a director
  , artDepartment :: Maybe ArtDepartment -- ^ Whether this person if from art department
  , publicationDesigner :: Maybe PublicationDesigner -- ^ Whether this person is a publication designer
  , comicInkArtist :: Maybe ComicInkArtist -- ^ Whether this person is a comic ink artist
  , comicArtist :: Maybe ComicArtist -- ^ Whether this person is a comic artist
  , referenceAuthor :: Maybe ReferenceAuthor -- ^ Whether this person is a reference author
  , cameraAndElectricalDepartment :: Maybe CameraAndElectricalDepartment -- ^ Whether this person is from camera and electrical department
  , exhibitAndAttractionStaff :: Maybe ExhibitAndAttractionStaff -- ^ Whether this person is an exhibit and tttraction staff
  , stuntDepartment :: Maybe StuntDepartment -- ^ Whether this person is from stunt department
  , uid :: Uid -- ^ Staff unique ID
  , writer :: Maybe Writer -- ^ Whether this person is a writer
  , audioAuthor :: Maybe AudioAuthor -- ^ Whether this person is an audio author
  , transportationDepartment :: Maybe TransportationDepartment -- ^ Whether this person is from transportation department
  , linguist :: Maybe Linguist -- ^ Whether this person is a linguist
  , cinematographer :: Maybe Cinematographer -- ^ Whether this person is a cinematographer
  , dateOfDeath :: Maybe DateOfDeath -- ^ Date the staff died
  , locationStaff :: Maybe LocationStaff -- ^ Whether this person is a location staff
  , calendarArtist :: Maybe CalendarArtist -- ^ Whether this person is a calendar artist
  , costumeDepartment :: Maybe CostumeDepartment -- ^ Whether this person is from costume department
  , novelArtist :: Maybe NovelArtist -- ^ Whether this person is a novel artist
  , artDirector :: Maybe ArtDirector -- ^ Whether this person is an art director
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
  , comicLetterArtist :: Maybe ComicLetterArtist -- ^ Whether this person is a comic letter artist
  , placeOfBirth :: Maybe PlaceOfBirth -- ^ Place the staff was born
  , productionDesigner :: Maybe ProductionDesigner -- ^ Whether this person is a production designer
  , videoGameProductionStaff :: Maybe VideoGameProductionStaff -- ^ Whether this person is video game production staff
  , productionStaff :: Maybe ProductionStaff -- ^ Whether this person is a production staff
  }
  deriving (Eq, Show)

staffBaseSchema :: FC.Fleece schema => schema StaffBase
staffBaseSchema =
  FC.object $
    FC.constructor StaffBase
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
      #+ FC.optional "assistantOrSecondUnitDirector" assistantOrSecondUnitDirector assistantOrSecondUnitDirectorSchema
      #+ FC.optional "gameAuthor" gameAuthor gameAuthorSchema
      #+ FC.optional "publicityArtist" publicityArtist publicityArtistSchema
      #+ FC.optional "publicationStaff" publicationStaff publicationStaffSchema
      #+ FC.optional "soundDepartment" soundDepartment soundDepartmentSchema
      #+ FC.optional "composer" composer composerSchema
      #+ FC.optional "publicationArtist" publicationArtist publicationArtistSchema
      #+ FC.optional "referenceArtist" referenceArtist referenceArtistSchema
      #+ FC.optional "comicColorArtist" comicColorArtist comicColorArtistSchema
      #+ FC.optional "comicAuthor" comicAuthor comicAuthorSchema
      #+ FC.optional "director" director directorSchema
      #+ FC.optional "artDepartment" artDepartment artDepartmentSchema
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
      #+ FC.optional "transportationDepartment" transportationDepartment transportationDepartmentSchema
      #+ FC.optional "linguist" linguist linguistSchema
      #+ FC.optional "cinematographer" cinematographer cinematographerSchema
      #+ FC.optional "dateOfDeath" dateOfDeath dateOfDeathSchema
      #+ FC.optional "locationStaff" locationStaff locationStaffSchema
      #+ FC.optional "calendarArtist" calendarArtist calendarArtistSchema
      #+ FC.optional "costumeDepartment" costumeDepartment costumeDepartmentSchema
      #+ FC.optional "novelArtist" novelArtist novelArtistSchema
      #+ FC.optional "artDirector" artDirector artDirectorSchema
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
      #+ FC.optional "comicLetterArtist" comicLetterArtist comicLetterArtistSchema
      #+ FC.optional "placeOfBirth" placeOfBirth placeOfBirthSchema
      #+ FC.optional "productionDesigner" productionDesigner productionDesignerSchema
      #+ FC.optional "videoGameProductionStaff" videoGameProductionStaff videoGameProductionStaffSchema
      #+ FC.optional "productionStaff" productionStaff productionStaffSchema