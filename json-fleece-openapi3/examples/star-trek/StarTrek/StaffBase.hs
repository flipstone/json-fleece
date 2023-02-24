{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffBase
  ( StaffBase(..)
  , staffBaseSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
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
  , dateOfDeath :: Maybe Text -- ^ Date the staff died
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
  , dateOfBirth :: Maybe Text -- ^ Date the staff was born
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
      #+ FC.optionalField FC.OmitKey_DelegateNull "studioExecutive" studioExecutive FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "birthName" birthName FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "gameArtist" gameArtist FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "filmEditor" filmEditor FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "comicStripArtist" comicStripArtist FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "castingDepartment" castingDepartment FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "costumeDesigner" costumeDesigner FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "author" author FC.boolean
      #+ FC.required "name" name FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "personalAssistant" personalAssistant FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "assistantOrSecondUnitDirector" assistantOrSecondUnitDirector FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "gameAuthor" gameAuthor FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "publicityArtist" publicityArtist FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "publicationStaff" publicationStaff FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "soundDepartment" soundDepartment FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "composer" composer FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "publicationArtist" publicationArtist FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "referenceArtist" referenceArtist FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "comicColorArtist" comicColorArtist FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "comicAuthor" comicAuthor FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "director" director FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "artDepartment" artDepartment FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "publicationDesigner" publicationDesigner FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "comicInkArtist" comicInkArtist FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "comicArtist" comicArtist FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "referenceAuthor" referenceAuthor FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "cameraAndElectricalDepartment" cameraAndElectricalDepartment FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "exhibitAndAttractionStaff" exhibitAndAttractionStaff FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "stuntDepartment" stuntDepartment FC.boolean
      #+ FC.required "uid" uid FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "writer" writer FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "audioAuthor" audioAuthor FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "transportationDepartment" transportationDepartment FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "linguist" linguist FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "cinematographer" cinematographer FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "dateOfDeath" dateOfDeath FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "locationStaff" locationStaff FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "calendarArtist" calendarArtist FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "costumeDepartment" costumeDepartment FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "novelArtist" novelArtist FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "artDirector" artDirector FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "productionAssociate" productionAssociate FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "cbsDigitalStaff" cbsDigitalStaff FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "storyEditor" storyEditor FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "comicInteriorArtist" comicInteriorArtist FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "specialFeaturesStaff" specialFeaturesStaff FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "scienceConsultant" scienceConsultant FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "gender" gender genderSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "dateOfBirth" dateOfBirth FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "publicationEditor" publicationEditor FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "placeOfDeath" placeOfDeath FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "novelAuthor" novelAuthor FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "ilmProductionStaff" ilmProductionStaff FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "producer" producer FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "musicDepartment" musicDepartment FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "comicPencilArtist" comicPencilArtist FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "specialAndVisualEffectsStaff" specialAndVisualEffectsStaff FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "makeupStaff" makeupStaff FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "comicLetterArtist" comicLetterArtist FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "placeOfBirth" placeOfBirth FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "productionDesigner" productionDesigner FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "videoGameProductionStaff" videoGameProductionStaff FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "productionStaff" productionStaff FC.boolean