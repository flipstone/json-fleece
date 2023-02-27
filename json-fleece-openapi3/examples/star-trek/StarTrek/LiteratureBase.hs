{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.LiteratureBase
  ( LiteratureBase(..)
  , literatureBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.LiteratureBase.EarthlyOrigin (EarthlyOrigin, earthlyOriginSchema)
import StarTrek.LiteratureBase.ReligiousLiterature (ReligiousLiterature, religiousLiteratureSchema)
import StarTrek.LiteratureBase.Report (Report, reportSchema)
import StarTrek.LiteratureBase.ScientificLiterature (ScientificLiterature, scientificLiteratureSchema)
import StarTrek.LiteratureBase.ShakespeareanWork (ShakespeareanWork, shakespeareanWorkSchema)
import StarTrek.LiteratureBase.TechnicalManual (TechnicalManual, technicalManualSchema)
import StarTrek.LiteratureBase.Title (Title, titleSchema)
import StarTrek.LiteratureBase.Uid (Uid, uidSchema)

data LiteratureBase = LiteratureBase
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

literatureBaseSchema :: FC.Fleece schema => schema LiteratureBase
literatureBaseSchema =
  FC.object $
    FC.constructor LiteratureBase
      #+ FC.optional "scientificLiterature" scientificLiterature scientificLiteratureSchema
      #+ FC.optional "shakespeareanWork" shakespeareanWork shakespeareanWorkSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "religiousLiterature" religiousLiterature religiousLiteratureSchema
      #+ FC.required "title" title titleSchema
      #+ FC.optional "technicalManual" technicalManual technicalManualSchema
      #+ FC.optional "earthlyOrigin" earthlyOrigin earthlyOriginSchema
      #+ FC.optional "report" report reportSchema