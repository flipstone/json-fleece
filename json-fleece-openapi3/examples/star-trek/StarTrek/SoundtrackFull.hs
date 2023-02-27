{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SoundtrackFull
  ( SoundtrackFull(..)
  , soundtrackFullSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Integer, Maybe, Show)
import StarTrek.CompanyBase (CompanyBase, companyBaseSchema)
import StarTrek.Reference (Reference, referenceSchema)
import StarTrek.StaffBase (StaffBase, staffBaseSchema)

data SoundtrackFull = SoundtrackFull
  { releaseDate :: Maybe Text -- ^ Release date
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
      #+ FC.optional "releaseDate" releaseDate FC.text
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "length" length FC.integer
      #+ FC.optional "composers" composers (FC.list staffBaseSchema)
      #+ FC.required "title" title FC.text
      #+ FC.optional "labels" labels (FC.list companyBaseSchema)
      #+ FC.optional "references" references (FC.list referenceSchema)
      #+ FC.optional "orchestrators" orchestrators (FC.list staffBaseSchema)
      #+ FC.optional "contributors" contributors (FC.list staffBaseSchema)