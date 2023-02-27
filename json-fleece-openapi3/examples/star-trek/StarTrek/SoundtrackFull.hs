{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SoundtrackFull
  ( SoundtrackFull(..)
  , soundtrackFullSchema
  ) where

import Data.Text (Text)
import Data.Time (Day)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Integer, Maybe, Show)
import StarTrek.CompanyBase (CompanyBase, companyBaseSchema)
import StarTrek.Reference (Reference, referenceSchema)
import StarTrek.StaffBase (StaffBase, staffBaseSchema)

data SoundtrackFull = SoundtrackFull
  { releaseDate :: Maybe Day -- ^ Release date
  , uid :: Text -- ^ Soundtrack unique ID
  , length :: Maybe Integer -- ^ Length, in seconds
  , composers :: Maybe [StaffBase] -- ^ Composers
  , title :: Text -- ^ Soundtrack title
  , labels :: Maybe [CompanyBase] -- ^ Labels this soundtrack was relesed by
  , references :: Maybe [Reference] -- ^ References
  , orchestrators :: Maybe [StaffBase] -- ^ Orchestrators
  , contributors :: Maybe [StaffBase] -- ^ Other musicians that contributed to this soundtrack
  }
  deriving (Eq, Show)

soundtrackFullSchema :: FC.Fleece schema => schema SoundtrackFull
soundtrackFullSchema =
  FC.object $
    FC.constructor SoundtrackFull
      #+ FC.optional "releaseDate" releaseDate FC.day
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "length" length FC.integer
      #+ FC.optional "composers" composers (FC.list staffBaseSchema)
      #+ FC.required "title" title FC.text
      #+ FC.optional "labels" labels (FC.list companyBaseSchema)
      #+ FC.optional "references" references (FC.list referenceSchema)
      #+ FC.optional "orchestrators" orchestrators (FC.list staffBaseSchema)
      #+ FC.optional "contributors" contributors (FC.list staffBaseSchema)