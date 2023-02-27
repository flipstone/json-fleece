{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffBase
  ( StaffBase(..)
  , staffBaseSchema
  ) where

import Data.Text (Text)
import Data.Time (Day)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Bool, Eq, Maybe, Show)
import StarTrek.Gender (Gender, genderSchema)

data StaffBase = StaffBase
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
  , assistantOrSecondUnitDirector :: Maybe Bool -- ^ Whether this person is an assistant or secound unit director director
  , gameAuthor :: Maybe Bool -- ^ Whether this person is a game author
  , publicityArtist :: Maybe Bool -- ^ Whether this person is a publication artist
  , publicationStaff :: Maybe Bool -- ^ Whether this person is a publication staff
  , soundDepartment :: Maybe Bool -- ^ Whether this person is from sound department
  , composer :: Maybe Bool -- ^ Whether this person is a composer
  , publicationArtist :: Maybe Bool -- ^ Whether this person is a publication artist
  , referenceArtist :: Maybe Bool -- ^ Whether this person is a reference artist
  , comicColorArtist :: Maybe Bool -- ^ Whether this person is a comic color artist
  , comicAuthor :: Maybe Bool -- ^ Whether this person is a comic author
  , director :: Maybe Bool -- ^ Whether this person is a director
  , artDepartment :: Maybe Bool -- ^ Whether this person if from art department
  , publicationDesigner :: Maybe Bool -- ^ Whether this person is a publication designer
  , comicInkArtist :: Maybe Bool -- ^ Whether this person is a comic ink artist
  , comicArtist :: Maybe Bool -- ^ Whether this person is a comic artist
  , referenceAuthor :: Maybe Bool -- ^ Whether this person is a reference author
  , cameraAndElectricalDepartment :: Maybe Bool -- ^ Whether this person is from camera and electrical department
  , exhibitAndAttractionStaff :: Maybe Bool -- ^ Whether this person is an exhibit and tttraction staff
  , stuntDepartment :: Maybe Bool -- ^ Whether this person is from stunt department
  , uid :: Text -- ^ Staff unique ID
  , writer :: Maybe Bool -- ^ Whether this person is a writer
  , audioAuthor :: Maybe Bool -- ^ Whether this person is an audio author
  , transportationDepartment :: Maybe Bool -- ^ Whether this person is from transportation department
  , linguist :: Maybe Bool -- ^ Whether this person is a linguist
  , cinematographer :: Maybe Bool -- ^ Whether this person is a cinematographer
  , dateOfDeath :: Maybe Day -- ^ Date the staff died
  , locationStaff :: Maybe Bool -- ^ Whether this person is a location staff
  , calendarArtist :: Maybe Bool -- ^ Whether this person is a calendar artist
  , costumeDepartment :: Maybe Bool -- ^ Whether this person is from costume department
  , novelArtist :: Maybe Bool -- ^ Whether this person is a novel artist
  , artDirector :: Maybe Bool -- ^ Whether this person is an art director
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
  , comicLetterArtist :: Maybe Bool -- ^ Whether this person is a comic letter artist
  , placeOfBirth :: Maybe Text -- ^ Place the staff was born
  , productionDesigner :: Maybe Bool -- ^ Whether this person is a production designer
  , videoGameProductionStaff :: Maybe Bool -- ^ Whether this person is video game production staff
  , productionStaff :: Maybe Bool -- ^ Whether this person is a production staff
  }
  deriving (Eq, Show)

staffBaseSchema :: FC.Fleece schema => schema StaffBase
staffBaseSchema =
  FC.object $
    FC.constructor StaffBase
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
      #+ FC.optional "assistantOrSecondUnitDirector" assistantOrSecondUnitDirector FC.boolean
      #+ FC.optional "gameAuthor" gameAuthor FC.boolean
      #+ FC.optional "publicityArtist" publicityArtist FC.boolean
      #+ FC.optional "publicationStaff" publicationStaff FC.boolean
      #+ FC.optional "soundDepartment" soundDepartment FC.boolean
      #+ FC.optional "composer" composer FC.boolean
      #+ FC.optional "publicationArtist" publicationArtist FC.boolean
      #+ FC.optional "referenceArtist" referenceArtist FC.boolean
      #+ FC.optional "comicColorArtist" comicColorArtist FC.boolean
      #+ FC.optional "comicAuthor" comicAuthor FC.boolean
      #+ FC.optional "director" director FC.boolean
      #+ FC.optional "artDepartment" artDepartment FC.boolean
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
      #+ FC.optional "transportationDepartment" transportationDepartment FC.boolean
      #+ FC.optional "linguist" linguist FC.boolean
      #+ FC.optional "cinematographer" cinematographer FC.boolean
      #+ FC.optional "dateOfDeath" dateOfDeath FC.day
      #+ FC.optional "locationStaff" locationStaff FC.boolean
      #+ FC.optional "calendarArtist" calendarArtist FC.boolean
      #+ FC.optional "costumeDepartment" costumeDepartment FC.boolean
      #+ FC.optional "novelArtist" novelArtist FC.boolean
      #+ FC.optional "artDirector" artDirector FC.boolean
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
      #+ FC.optional "comicLetterArtist" comicLetterArtist FC.boolean
      #+ FC.optional "placeOfBirth" placeOfBirth FC.text
      #+ FC.optional "productionDesigner" productionDesigner FC.boolean
      #+ FC.optional "videoGameProductionStaff" videoGameProductionStaff FC.boolean
      #+ FC.optional "productionStaff" productionStaff FC.boolean