{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.LiteratureFull
  ( LiteratureFull(..)
  , literatureFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.LiteratureFull.EarthlyOrigin as EarthlyOrigin
import qualified StarTrek.Types.LiteratureFull.ReligiousLiterature as ReligiousLiterature
import qualified StarTrek.Types.LiteratureFull.Report as Report
import qualified StarTrek.Types.LiteratureFull.ScientificLiterature as ScientificLiterature
import qualified StarTrek.Types.LiteratureFull.ShakespeareanWork as ShakespeareanWork
import qualified StarTrek.Types.LiteratureFull.TechnicalManual as TechnicalManual
import qualified StarTrek.Types.LiteratureFull.Title as Title
import qualified StarTrek.Types.LiteratureFull.Uid as Uid

data LiteratureFull = LiteratureFull
  { scientificLiterature :: Maybe ScientificLiterature.ScientificLiterature -- ^ Whether it's a scientific literature
  , report :: Maybe Report.Report -- ^ Whether it's a report
  , religiousLiterature :: Maybe ReligiousLiterature.ReligiousLiterature -- ^ Whether it's a religious literature
  , uid :: Uid.Uid -- ^ Literature unique ID
  , shakespeareanWork :: Maybe ShakespeareanWork.ShakespeareanWork -- ^ Whether it's a Shakespearean work
  , technicalManual :: Maybe TechnicalManual.TechnicalManual -- ^ Whether it's a technical manual
  , title :: Title.Title -- ^ Literature title
  , earthlyOrigin :: Maybe EarthlyOrigin.EarthlyOrigin -- ^ Whether it's of earthly origin
  }
  deriving (Eq, Show)

literatureFullSchema :: FC.Fleece schema => schema LiteratureFull
literatureFullSchema =
  FC.object $
    FC.constructor LiteratureFull
      #+ FC.optional "scientificLiterature" scientificLiterature ScientificLiterature.scientificLiteratureSchema
      #+ FC.optional "report" report Report.reportSchema
      #+ FC.optional "religiousLiterature" religiousLiterature ReligiousLiterature.religiousLiteratureSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "shakespeareanWork" shakespeareanWork ShakespeareanWork.shakespeareanWorkSchema
      #+ FC.optional "technicalManual" technicalManual TechnicalManual.technicalManualSchema
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "earthlyOrigin" earthlyOrigin EarthlyOrigin.earthlyOriginSchema