{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.LiteratureBase
  ( LiteratureBase(..)
  , literatureBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.LiteratureBase.EarthlyOrigin as EarthlyOrigin
import qualified StarTrek.Types.LiteratureBase.ReligiousLiterature as ReligiousLiterature
import qualified StarTrek.Types.LiteratureBase.Report as Report
import qualified StarTrek.Types.LiteratureBase.ScientificLiterature as ScientificLiterature
import qualified StarTrek.Types.LiteratureBase.ShakespeareanWork as ShakespeareanWork
import qualified StarTrek.Types.LiteratureBase.TechnicalManual as TechnicalManual
import qualified StarTrek.Types.LiteratureBase.Title as Title
import qualified StarTrek.Types.LiteratureBase.Uid as Uid

data LiteratureBase = LiteratureBase
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

literatureBaseSchema :: FC.Fleece schema => schema LiteratureBase
literatureBaseSchema =
  FC.object $
    FC.constructor LiteratureBase
      #+ FC.optional "scientificLiterature" scientificLiterature ScientificLiterature.scientificLiteratureSchema
      #+ FC.optional "report" report Report.reportSchema
      #+ FC.optional "religiousLiterature" religiousLiterature ReligiousLiterature.religiousLiteratureSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "shakespeareanWork" shakespeareanWork ShakespeareanWork.shakespeareanWorkSchema
      #+ FC.optional "technicalManual" technicalManual TechnicalManual.technicalManualSchema
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "earthlyOrigin" earthlyOrigin EarthlyOrigin.earthlyOriginSchema