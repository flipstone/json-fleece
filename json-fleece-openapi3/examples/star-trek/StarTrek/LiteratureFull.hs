{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.LiteratureFull
  ( LiteratureFull(..)
  , literatureFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.LiteratureFull.EarthlyOrigin (EarthlyOrigin, earthlyOriginSchema)
import StarTrek.LiteratureFull.ReligiousLiterature (ReligiousLiterature, religiousLiteratureSchema)
import StarTrek.LiteratureFull.Report (Report, reportSchema)
import StarTrek.LiteratureFull.ScientificLiterature (ScientificLiterature, scientificLiteratureSchema)
import StarTrek.LiteratureFull.ShakespeareanWork (ShakespeareanWork, shakespeareanWorkSchema)
import StarTrek.LiteratureFull.TechnicalManual (TechnicalManual, technicalManualSchema)
import StarTrek.LiteratureFull.Title (Title, titleSchema)
import StarTrek.LiteratureFull.Uid (Uid, uidSchema)

data LiteratureFull = LiteratureFull
  { scientificLiterature :: Maybe ScientificLiterature -- ^ Whether it's a scientific literature
  , shakespeareanWork :: Maybe ShakespeareanWork -- ^ Whether it's a Shakespearean work
  , uid :: Uid -- ^ Literature unique ID
  , religiousLiterature :: Maybe ReligiousLiterature -- ^ Whether it's a religious literature
  , title :: Title -- ^ Literature title
  , technicalManual :: Maybe TechnicalManual -- ^ Whether it's a technical manual
  , earthlyOrigin :: Maybe EarthlyOrigin -- ^ Whether it's of earthly origin
  , report :: Maybe Report -- ^ Whether it's a report
  }
  deriving (Eq, Show)

literatureFullSchema :: FC.Fleece schema => schema LiteratureFull
literatureFullSchema =
  FC.object $
    FC.constructor LiteratureFull
      #+ FC.optional "scientificLiterature" scientificLiterature scientificLiteratureSchema
      #+ FC.optional "shakespeareanWork" shakespeareanWork shakespeareanWorkSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "religiousLiterature" religiousLiterature religiousLiteratureSchema
      #+ FC.required "title" title titleSchema
      #+ FC.optional "technicalManual" technicalManual technicalManualSchema
      #+ FC.optional "earthlyOrigin" earthlyOrigin earthlyOriginSchema
      #+ FC.optional "report" report reportSchema