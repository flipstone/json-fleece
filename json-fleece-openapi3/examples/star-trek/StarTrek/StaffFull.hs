{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffFull
  ( StaffFull(..)
  , staffFullSchema
  ) where

import Data.Text (Text)
import Data.Time (Day)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Bool, Eq, Maybe, Show)
import StarTrek.EpisodeBase (EpisodeBase, episodeBaseSchema)
import StarTrek.Gender (Gender, genderSchema)
import StarTrek.MovieBase (MovieBase, movieBaseSchema)

data StaffFull = StaffFull
  { studioExecutive :: Maybe Bool -- ^ Whether this person is a studio executive
  , birthName :: Maybe Text -- ^ Staff birth name
  , gameArtist :: Maybe Bool -- ^ Whether this person is a game artist
  , filmEditor :: Maybe Bool -- ^ Whether this person is a film editor
  , comicStripArtist :: Maybe Bool -- ^ Whether this person is a comic strip artist
  , castingDepartment :: Maybe Bool -- ^ Whether this person is from casting department
  , costumeDesigner :: Maybe Bool -- ^ Whether this person is a custume designer
  , author :: Maybe Bool -- ^ Whether this person is an author
  , name :: Text -- ^ Staff name
  , personalAssistant :: Maybe Bool -- ^ Whether this person is a personal assistant
  , producedMovies :: Maybe [MovieBase] -- ^ Movies produced by this person
  , assistantOrSecondUnitDirector :: Maybe Bool -- ^ Whether this person is an assistant or secound unit director director
  , gameAuthor :: Maybe Bool -- ^ Whether this person is a game author
  , publicityArtist :: Maybe Bool -- ^ Whether this person is a publicity artist
  , directedMovies :: Maybe [MovieBase] -- ^ Movies directed by this person
  , teleplayAuthoredEpisodes :: Maybe [EpisodeBase] -- ^ Episodes to which this person has written teleplay
  , publicationStaff :: Maybe Bool -- ^ Whether this person is a publication staff
  , soundDepartment :: Maybe Bool -- ^ Whether this person is from sound department
  , composer :: Maybe Bool -- ^ Whether this person is a composer
  , publicationArtist :: Maybe Bool -- ^ Whether this person is a publication artist
  , referenceArtist :: Maybe Bool -- ^ Whether this person is a reference artist
  , writtenMovies :: Maybe [MovieBase] -- ^ Movies written by this person
  , comicColorArtist :: Maybe Bool -- ^ Whether this person is a comic color artist
  , comicAuthor :: Maybe Bool -- ^ Whether this person is a comic author
  , director :: Maybe Bool -- ^ Whether this person is a director
  , artDepartment :: Maybe Bool -- ^ Whether this person is from art department
  , writtenEpisodes :: Maybe [EpisodeBase] -- ^ Episodes written by this person
  , publicationDesigner :: Maybe Bool -- ^ Whether this person is a publication designer
  , comicInkArtist :: Maybe Bool -- ^ Whether this person is a comic ink artist
  , comicArtist :: Maybe Bool -- ^ Whether this person is a comic artist
  , referenceAuthor :: Maybe Bool -- ^ Whether this person is a reference author
  , cameraAndElectricalDepartment :: Maybe Bool -- ^ Whether this person is from camera and electrical department
  , exhibitAndAttractionStaff :: Maybe Bool -- ^ Whether this person is an exhibit and attraction staff
  , stuntDepartment :: Maybe Bool -- ^ Whether this person is from stunt department
  , uid :: Text -- ^ Staff unique ID
  , writer :: Maybe Bool -- ^ Whether this person is a writer
  , audioAuthor :: Maybe Bool -- ^ Whether this person is an audio author
  , episodes :: Maybe [EpisodeBase] -- ^ Episodes on which this person worked
  , transportationDepartment :: Maybe Bool -- ^ Whether this person is from transportation department
  , screenplayAuthoredMovies :: Maybe [MovieBase] -- ^ Movies to which this person has written screenplay
  , linguist :: Maybe Bool -- ^ Whether this person is a linguist
  , cinematographer :: Maybe Bool -- ^ Whether this person is a cinematographer
  , dateOfDeath :: Maybe Day -- ^ Date the staff died
  , locationStaff :: Maybe Bool -- ^ Whether this person is a location staff
  , storyAuthoredMovies :: Maybe [MovieBase] -- ^ Movies to which this person has written story
  , calendarArtist :: Maybe Bool -- ^ Whether this person is a calendar artist
  , costumeDepartment :: Maybe Bool -- ^ Whether this person is from costume department
  , novelArtist :: Maybe Bool -- ^ Whether this person is a novel artist
  , artDirector :: Maybe Bool -- ^ Whether this person is an art director
  , directedEpisodes :: Maybe [EpisodeBase] -- ^ Episodes directed by this person
  , productionAssociate :: Maybe Bool -- ^ Whether this person is a production associate
  , cbsDigitalStaff :: Maybe Bool -- ^ Whether this person is a part of CBS digital staff
  , storyEditor :: Maybe Bool -- ^ Whether this person is a story editor
  , comicInteriorArtist :: Maybe Bool -- ^ Whether this person is a comic interior artist
  , specialFeaturesStaff :: Maybe Bool -- ^ Whether this person is a special features artist
  , scienceConsultant :: Maybe Bool -- ^ Whether this person is a science consultant
  , gender :: Maybe Gender -- ^ Gender
  , dateOfBirth :: Maybe Day -- ^ Date the staff was born
  , publicationEditor :: Maybe Bool -- ^ Whether this person is a publication editor
  , placeOfDeath :: Maybe Text -- ^ Place the staff died
  , novelAuthor :: Maybe Bool -- ^ Whether this person is a novel author
  , ilmProductionStaff :: Maybe Bool -- ^ Whether this person is a part of ILM production staff
  , producer :: Maybe Bool -- ^ Whether this person is a producer
  , musicDepartment :: Maybe Bool -- ^ Whether this person is from music department
  , comicPencilArtist :: Maybe Bool -- ^ Whether this person is a comic pencil artist
  , specialAndVisualEffectsStaff :: Maybe Bool -- ^ Whether this person is a special and visual effects staff
  , makeupStaff :: Maybe Bool -- ^ Whether this person is a make-up staff
  , movies :: Maybe [MovieBase] -- ^ Movies on which this person worked
  , storyAuthoredEpisodes :: Maybe [EpisodeBase] -- ^ Episodes to which this person has written story
  , comicLetterArtist :: Maybe Bool -- ^ Whether this person is a comic letter artist
  , placeOfBirth :: Maybe Text -- ^ Place the staff was born
  , productionDesigner :: Maybe Bool -- ^ Whether this person is a production designer
  , videoGameProductionStaff :: Maybe Bool -- ^ Whether this person is video game production staff
  , productionStaff :: Maybe Bool -- ^ Whether this person is a production staff
  }
  deriving (Eq, Show)

staffFullSchema :: FC.Fleece schema => schema StaffFull
staffFullSchema =
  FC.object $
    FC.constructor StaffFull
      #+ FC.optional "studioExecutive" studioExecutive FC.boolean
      #+ FC.optional "birthName" birthName FC.text
      #+ FC.optional "gameArtist" gameArtist FC.boolean
      #+ FC.optional "filmEditor" filmEditor FC.boolean
      #+ FC.optional "comicStripArtist" comicStripArtist FC.boolean
      #+ FC.optional "castingDepartment" castingDepartment FC.boolean
      #+ FC.optional "costumeDesigner" costumeDesigner FC.boolean
      #+ FC.optional "author" author FC.boolean
      #+ FC.required "name" name FC.text
      #+ FC.optional "personalAssistant" personalAssistant FC.boolean
      #+ FC.optional "producedMovies" producedMovies (FC.list movieBaseSchema)
      #+ FC.optional "assistantOrSecondUnitDirector" assistantOrSecondUnitDirector FC.boolean
      #+ FC.optional "gameAuthor" gameAuthor FC.boolean
      #+ FC.optional "publicityArtist" publicityArtist FC.boolean
      #+ FC.optional "directedMovies" directedMovies (FC.list movieBaseSchema)
      #+ FC.optional "teleplayAuthoredEpisodes" teleplayAuthoredEpisodes (FC.list episodeBaseSchema)
      #+ FC.optional "publicationStaff" publicationStaff FC.boolean
      #+ FC.optional "soundDepartment" soundDepartment FC.boolean
      #+ FC.optional "composer" composer FC.boolean
      #+ FC.optional "publicationArtist" publicationArtist FC.boolean
      #+ FC.optional "referenceArtist" referenceArtist FC.boolean
      #+ FC.optional "writtenMovies" writtenMovies (FC.list movieBaseSchema)
      #+ FC.optional "comicColorArtist" comicColorArtist FC.boolean
      #+ FC.optional "comicAuthor" comicAuthor FC.boolean
      #+ FC.optional "director" director FC.boolean
      #+ FC.optional "artDepartment" artDepartment FC.boolean
      #+ FC.optional "writtenEpisodes" writtenEpisodes (FC.list episodeBaseSchema)
      #+ FC.optional "publicationDesigner" publicationDesigner FC.boolean
      #+ FC.optional "comicInkArtist" comicInkArtist FC.boolean
      #+ FC.optional "comicArtist" comicArtist FC.boolean
      #+ FC.optional "referenceAuthor" referenceAuthor FC.boolean
      #+ FC.optional "cameraAndElectricalDepartment" cameraAndElectricalDepartment FC.boolean
      #+ FC.optional "exhibitAndAttractionStaff" exhibitAndAttractionStaff FC.boolean
      #+ FC.optional "stuntDepartment" stuntDepartment FC.boolean
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "writer" writer FC.boolean
      #+ FC.optional "audioAuthor" audioAuthor FC.boolean
      #+ FC.optional "episodes" episodes (FC.list episodeBaseSchema)
      #+ FC.optional "transportationDepartment" transportationDepartment FC.boolean
      #+ FC.optional "screenplayAuthoredMovies" screenplayAuthoredMovies (FC.list movieBaseSchema)
      #+ FC.optional "linguist" linguist FC.boolean
      #+ FC.optional "cinematographer" cinematographer FC.boolean
      #+ FC.optional "dateOfDeath" dateOfDeath FC.day
      #+ FC.optional "locationStaff" locationStaff FC.boolean
      #+ FC.optional "storyAuthoredMovies" storyAuthoredMovies (FC.list movieBaseSchema)
      #+ FC.optional "calendarArtist" calendarArtist FC.boolean
      #+ FC.optional "costumeDepartment" costumeDepartment FC.boolean
      #+ FC.optional "novelArtist" novelArtist FC.boolean
      #+ FC.optional "artDirector" artDirector FC.boolean
      #+ FC.optional "directedEpisodes" directedEpisodes (FC.list episodeBaseSchema)
      #+ FC.optional "productionAssociate" productionAssociate FC.boolean
      #+ FC.optional "cbsDigitalStaff" cbsDigitalStaff FC.boolean
      #+ FC.optional "storyEditor" storyEditor FC.boolean
      #+ FC.optional "comicInteriorArtist" comicInteriorArtist FC.boolean
      #+ FC.optional "specialFeaturesStaff" specialFeaturesStaff FC.boolean
      #+ FC.optional "scienceConsultant" scienceConsultant FC.boolean
      #+ FC.optional "gender" gender genderSchema
      #+ FC.optional "dateOfBirth" dateOfBirth FC.day
      #+ FC.optional "publicationEditor" publicationEditor FC.boolean
      #+ FC.optional "placeOfDeath" placeOfDeath FC.text
      #+ FC.optional "novelAuthor" novelAuthor FC.boolean
      #+ FC.optional "ilmProductionStaff" ilmProductionStaff FC.boolean
      #+ FC.optional "producer" producer FC.boolean
      #+ FC.optional "musicDepartment" musicDepartment FC.boolean
      #+ FC.optional "comicPencilArtist" comicPencilArtist FC.boolean
      #+ FC.optional "specialAndVisualEffectsStaff" specialAndVisualEffectsStaff FC.boolean
      #+ FC.optional "makeupStaff" makeupStaff FC.boolean
      #+ FC.optional "movies" movies (FC.list movieBaseSchema)
      #+ FC.optional "storyAuthoredEpisodes" storyAuthoredEpisodes (FC.list episodeBaseSchema)
      #+ FC.optional "comicLetterArtist" comicLetterArtist FC.boolean
      #+ FC.optional "placeOfBirth" placeOfBirth FC.text
      #+ FC.optional "productionDesigner" productionDesigner FC.boolean
      #+ FC.optional "videoGameProductionStaff" videoGameProductionStaff FC.boolean
      #+ FC.optional "productionStaff" productionStaff FC.boolean